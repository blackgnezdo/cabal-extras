module CabalBundler.OpenBSD (
    generateOpenBSD,
)  where

import Peura

import Data.List (intercalate, partition)

import qualified Cabal.Index                            as I
import qualified Cabal.Plan                             as P
import qualified Data.Map.Strict                        as M
import qualified Data.Text                              as T
import qualified Distribution.Types.PackageName         as C
import qualified Distribution.Types.UnqualComponentName as C
import qualified Distribution.Types.Version             as C
import qualified Distribution.Version                   as C

import CabalBundler.ExeOption

generateOpenBSD
    :: TracerPeu r w
    -> PackageName
    -> ExeOption C.UnqualComponentName
    -> P.PlanJson
    -> Map PackageName I.PackageInfo
    -> Peu r String
generateOpenBSD tracer packageName exeName' plan meta = do
    exeName <- case exeName' of
        ExeOptionPkg x -> return $ C.unUnqualComponentName x
        ExeOption x    -> return $ C.unUnqualComponentName x
        ExeOptionAll   -> die tracer "--exe-all isn't supported for openbsd output"

    let units :: Map P.UnitId P.Unit
        units = P.pjUnits plan

    case findExe packageName exeName units of
        [(uid0, pkgId0)] -> do
            -- Collect all global units from the plan (not just those reachable from executable)
            -- This ensures test dependencies like tasty are included
            let allGlobalUnits =
                    [ unit
                    | unit <- M.elems units
                    , P.uType unit == P.UnitTypeGlobal
                    , case P.uPkgSrc unit of
                        Just (P.RepoTarballPackage _) -> True
                        _                             -> False
                    ]
            deps <- unitsToDeps meta allGlobalUnits
            -- Apply version overrides for known problematic packages
            fixedDeps <- applyVersionOverrides meta deps
            case partition ((pkgId0 == ) . depPkgId) fixedDeps of
                (mainPackage : _, depUnits) -> do
                    return $ unlines $ makefileLines mainPackage depUnits
                ([], _) -> do
                    die tracer $ "Expected to find main package " <>
                        show (pkgId0, depPkgId <$> fixedDeps)
        uids ->
              throwM $ UnknownExecutable exeName (fst <$> uids)

unitsToDeps :: Map PackageName I.PackageInfo -> [P.Unit] -> Peu r [Dep]
unitsToDeps meta units = fmap concat $ for units $ \unit -> do
    let P.PkgId (P.PkgName tpkgname) (P.Ver verdigits) = P.uPId unit

    let cpkgname :: C.PackageName
        cpkgname = C.mkPackageName (T.unpack tpkgname)

    let cversion :: C.Version
        cversion = C.mkVersion verdigits

    rev <- case P.uType unit of
        P.UnitTypeBuiltin -> pure Nothing
        P.UnitTypeLocal   -> pure $ Just  0  -- Revision unavailable for local packages
        t -> do
            case P.uSha256 unit of
                Just _  -> do
                    pkgInfo <- maybe (throwM $ UnknownPackageName cpkgname) return $
                        M.lookup cpkgname meta
                    relInfo <- maybe (throwM $ UnknownPackageVersion cpkgname cversion) return $
                        M.lookup cversion $ I.piVersions pkgInfo

                    pure $ Just $ fromIntegral (I.riRevision relInfo)

                Nothing -> throwM $ UnknownUnitType cpkgname t

    let depForRev r = [Dep {
          depPackageName = cpkgname
        , depVersion     = cversion
        , depRevision    = r
        , depPkgId       = P.uPId unit
        }]
    pure $ maybe [] depForRev rev

data MetadataException
    = UnknownPackageName C.PackageName
    | UnknownExecutable String [P.UnitId]
    | UnknownPackageVersion C.PackageName C.Version
    | UnknownUnitType C.PackageName P.UnitType
  deriving Show

instance Exception MetadataException

-------------------------------------------------------------------------------
-- Version overrides for problematic packages
-------------------------------------------------------------------------------

-- | Map of (package name, bad version) to good version
--
-- Add problematic package versions here to automatically substitute them.
-- When a package version is known to be broken on OpenBSD, add an entry here
-- to replace it with a known good version.
--
-- Example: If splitmix-0.1.3.1 is broken, it will be replaced with 0.1.2:
--
-- > [ (mkPackageName "splitmix", C.mkVersion [0,1,3,1]) =: C.mkVersion [0,1,2]
-- > , (mkPackageName "another-pkg", C.mkVersion [1,2,3]) =: C.mkVersion [1,2,2]
-- > ]
versionOverrides :: Map (PackageName, Version) Version
versionOverrides = M.fromList
    [ (mkPackageName "splitmix", C.mkVersion [0,1,3,1]) =: C.mkVersion [0,1,2]
    ]
  where
    (=:) = (,)

applyVersionOverrides :: Map PackageName I.PackageInfo -> [Dep] -> Peu r [Dep]
applyVersionOverrides meta = traverse applyOverride
  where
    applyOverride dep =
        case M.lookup (depPackageName dep, depVersion dep) versionOverrides of
            Just newVersion -> do
                -- Look up the revision for the new version
                pkgInfo <- maybe (throwM $ UnknownPackageName (depPackageName dep)) return $
                    M.lookup (depPackageName dep) meta
                relInfo <- maybe (throwM $ UnknownPackageVersion (depPackageName dep) newVersion) return $
                    M.lookup newVersion (I.piVersions pkgInfo)

                let newPkgId = P.PkgId (fromCabal (depPackageName dep)) (fromCabal newVersion)

                return dep
                    { depVersion = newVersion
                    , depRevision = fromIntegral (I.riRevision relInfo)
                    , depPkgId = newPkgId
                    }
            Nothing -> return dep

-------------------------------------------------------------------------------
-- Output formatting
-------------------------------------------------------------------------------

makefileLines :: Dep -> [Dep] -> [String]
makefileLines mainPackage deps =
    let cleanedDeps = ordNubOn depPackageName $ sortOn depPackageName deps
    in  [ "MODCABAL_STEM\t\t= " <> prettyShow (depPackageName mainPackage)
        , "MODCABAL_VERSION\t= " <>  prettyShow (depVersion mainPackage)
        ] <>
        [ "MODCABAL_REVISION\t= " <> show rev
        | let rev = depRevision mainPackage
        , rev > 0]
        <>
        [ "MODCABAL_MANIFEST\t= \\" | not $ null cleanedDeps ] <>
        map manifestLine cleanedDeps

manifestLine :: Dep -> String
manifestLine dep =
    let name = prettyShow $ depPackageName dep
        ver = prettyShow $ depVersion dep
        rev = show $ depRevision dep
     in intercalate "\t" ["", name, ver, rev, "\\"]

findExe :: PackageName ->  String -> Map P.UnitId P.Unit -> [(P.UnitId, P.PkgId)]
findExe pn exeName units =
    [ (uid, P.uPId unit)
    | (uid, unit) <- M.toList units
    , pkgName (toCabal (P.uPId unit)) == pn
    , (P.CompNameExe e, _) <- M.toList (P.uComps unit)
    , e == T.pack exeName
    ]

data Dep = Dep
    { depPackageName :: PackageName
    , depVersion     :: Version
    , depRevision    :: Word
    , depPkgId       :: P.PkgId
    }
  deriving (Show)
