module GetCore where

import GHC
import GHC.Paths (libdir)
import DynFlags
import HscMain
import HscTypes
import MonadUtils
import MkExternalCore
import Panic
import TidyPgm

import Data.List (find)
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

tmpCoreFileName :: String
tmpCoreFileName = "core.hcr"

getCore :: FilePath -> IO Text
getCore filename =
    withSystemTempDirectory "halicore.XXXXX" $ \tmpDir -> do
        defaultErrorHandler defaultLogAction $ do
            coreText <- runGhc (Just libdir) $ getCore' tmpDir filename
            return coreText

getCore' :: GhcMonad m => FilePath -> FilePath -> m Text
getCore' tmpDir filename = do
    dflags <- getSessionDynFlags
    let coreFile = tmpDir </> tmpCoreFileName
    let dflags' = halicoreDynFlags tmpDir coreFile dflags
    setSessionDynFlags dflags'
    cg_guts <- getCgGuts filename
    liftIO $ emitExternalCore dflags' cg_guts
    coreText <- liftIO $ T.readFile coreFile
    return coreText

-- copied from GHC
getCgGuts :: GhcMonad m => FilePath -> m CgGuts
getCgGuts fn = do
    -- First, set the target to the desired filename
    target <- guessTarget fn Nothing
    addTarget target
    _ <- load LoadAllTargets
    -- Then find dependencies
    modGraph <- depanal [] True
    case find ((== fn) . msHsFilePath) modGraph of
        Just modSummary -> do
            -- Now we have the module name;
            -- parse, typecheck and desugar the module
            mod_guts <- coreModule `fmap`
                (desugarModule =<< typecheckModule =<< parseModule modSummary)
            hsc_env <- getSession
            simpl_guts <- liftIO $ hscSimplify hsc_env mod_guts
            (cg_guts, _) <- liftIO $ tidyProgram hsc_env simpl_guts
            return $ cg_guts
        Nothing -> panic "compileToCoreModule: target FilePath not found in module dependency graph"

halicoreDynFlags :: FilePath -> FilePath -> DynFlags -> DynFlags
halicoreDynFlags tmpDir coreFile dflags
    = dopt_set dflags' Opt_EmitExternalCore
  where
    dflags' = dflags
            { objectDir   = Just tmpDir
            , hiDir       = Just tmpDir
            , stubDir     = Just tmpDir
            , dumpDir     = Just tmpDir
            , extCoreName = coreFile
            }

