module Main where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Text.PrettyPrint.Leijen.Text (hPutDoc)

import GetCore
import Language.Core.Parser
import Language.Core.ParseGlue
import Language.Core.Isabelle

data Options = Options
    { optPrintCore :: Bool
    , optOutfile   :: FilePath
    } deriving (Eq, Show)

initialOptions :: Options
initialOptions = Options
    { optPrintCore = False
    , optOutfile   = "-"
    }

type OptAction = Options -> IO Options

options :: [OptDescr OptAction]
options =
    [ Option ['v'] ["version"]
        (NoArg (\_ -> do
            prg <- getProgName
            putStrLn (versionInfo prg)
            exitWith ExitSuccess))
        "Print version"
    
    , Option ['h', '?'] ["help"]
        (NoArg (\_ -> do
            prg <- getProgName
            putStr (usageInfo (usageHeader prg) options)
            mapM_ putStrLn description
            exitWith ExitSuccess))
        "Print help"

    , Option ['S'] []
        (NoArg (\opt ->
            return opt { optPrintCore = True }))
        "Stop after obtaining GHC Core"

    , Option ['o'] []
        (ReqArg (\file opt ->
            return opt { optOutfile = file }) "FILE")
        "Set the output filename"
    ]

halicoreOpts :: [String] -> IO ([OptAction], [String])
halicoreOpts argv = case getOpt' Permute options argv of
    (o, n, _, []) -> return (o, n)
    (_, _, _, errs) -> usageError errs

versionInfo :: String -> String
versionInfo prg = prg ++ " 0.1.0"

description :: [String]
description = 
    [ ""
    , "halicore translates GHC Core into Isabelle/HOLCF"
    , "<file> can be a Haskell file (.hs) or GHC Core file (.hcr)"
    ]

usageHeader :: String -> String
usageHeader prg = "Usage: " ++ prg ++ " [options] <file>"

usageError :: [String] -> IO a
usageError errs = do
    prg <- getProgName
    mapM_ errMsg errs
    putStrLn $ "Type " ++ prg ++ " --help for more information."
    exitFailure

errMsg :: String -> IO ()
errMsg err = do
    prg <- getProgName
    putStr prg
    putStr ": error: "
    putStr err

main :: IO ()
main = do
    args <- getArgs
    (actions, otherOpts) <- halicoreOpts args
    opts <- foldl (>>=) (return initialOptions) actions

    infile <- case otherOpts of
        []  -> usageError ["missing required <file> argument\n"]
        [f] -> return f
        _   -> usageError ["only one file must be specified\n"]

    exists <- doesFileExist infile
    when (not exists) $ do
        errMsg $ "file not found: " ++ infile ++ "\n"
        exitFailure

    coreText <- case takeExtension infile of
        ".hs"  -> getCore infile
        ".hcr" -> T.readFile infile
        _      -> usageError ["unsupported file type: " ++ infile ++ "\n"]

    outhandle <- case optOutfile opts of
        "-" -> return stdout
        fn  -> openFile fn WriteMode

    when (optPrintCore opts) $ do
        T.hPutStr outhandle coreText
        hClose outhandle
        exitSuccess

    -- translate to HOLCF
    let newName = takeBaseName infile
    thyDoc <- case parse (T.unpack coreText) 0 of
        FailP s -> errMsg s >> exitFailure
        OkP m -> return $ processModule m newName

    hPutDoc outhandle thyDoc
    hClose outhandle

    exitSuccess
