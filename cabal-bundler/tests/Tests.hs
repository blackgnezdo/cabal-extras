module Main (main) where

import Peura
import Prelude ()

import Data.List         (isPrefixOf)
import Test.Tasty        (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)

import qualified Cabal.Index      as I
import qualified Cabal.Plan       as P
import qualified System.Directory as Dir
import qualified System.FilePath  as FP

import CabalBundler.Curl      (generateCurl)
import CabalBundler.NixSingle (generateDerivationNix)
import CabalBundler.OpenBSD   (generateOpenBSD)

main :: IO ()
main = do
    (_, meta) <- liftIO I.cachedHackageMetadata

    cwd <- Dir.getCurrentDirectory
    let pwd = case reverse (FP.splitPath cwd) of
            segment : _ | "cabal-bundler" `isPrefixOf` segment -> cwd
            _                                                  -> cwd FP.</> "cabal-bundler"

    let nullTracer' = nullTracer :: TracerPeu () Void

    let golden :: TestName -> Peu () ByteString -> TestTree
        golden name action = goldenVsStringDiff
            name
            diffProc
            (pwd FP.</> "fixtures" FP.</> name)
            (runPeu nullTracer' () (fmap toLazy action))

    let pn      = mkPackageName "cabal-fmt"
        exeName = "cabal-fmt"

    defaultMain $ testGroup "cabal-bundler"
        [ golden "derivation.nix" $ do
            planPath <- makeAbsoluteFilePath $ pwd FP.</> "fixtures/cabal-fmt.plan.json"
            plan     <- liftIO $ P.decodePlanJson (toFilePath planPath)
            script   <- generateDerivationNix pn exeName plan meta

            return (toUTF8BS script)

        , golden "fetch-with-curl.sh"$ do
            planPath <- makeAbsoluteFilePath $ pwd FP.</> "fixtures/cabal-fmt.plan.json"
            plan     <- liftIO $ P.decodePlanJson (toFilePath planPath)
            script   <- generateCurl pn exeName plan meta

            return (toUTF8BS script)

        , golden "openbsd-ports.txt" $ do
            planPath <- makeAbsoluteFilePath $ pwd FP.</> "fixtures/cabal-fmt.plan.json"
            plan     <- liftIO $ P.decodePlanJson (toFilePath planPath)
            script   <- generateOpenBSD nullTracer' pn exeName plan meta

            return (toUTF8BS script)
        ]
  where
    diffProc ref new = ["diff", "-u", ref, new]

