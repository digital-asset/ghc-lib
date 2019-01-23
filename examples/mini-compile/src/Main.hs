{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main (main) where

import "ghc-lib" GHC
import "ghc-lib" Paths_ghc_lib
import "ghc-lib" HeaderInfo
import "ghc-lib" Module
import "ghc-lib" Config
import "ghc-lib" DynFlags
import "ghc-lib" Platform
import "ghc-lib" StringBuffer
import "ghc-lib" Fingerprint

import Outputable
import System.Environment
import System.IO.Extra
import qualified Data.Map.Strict as Map
import Data.IORef

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      s <- readFile' file
      flags <- mkDynFlags file s
      dataDir <- getDataDir
      cm <- runGhc (Just dataDir)
              (setSessionDynFlags flags
               >> return file >>= compileToCoreSimplified)
      putStrLn (showSDocUnsafe (ppr (cm)))
    _ -> fail "Exactly one file argument required"

mkDynFlags :: String -> String -> IO DynFlags
mkDynFlags filename s = do {
  ; dirs_to_clean <- newIORef Map.empty
  ; files_to_clean <- newIORef emptyFilesToClean
  ; next_temp_suffix <- newIORef 0
  ; let baseFlags =
          (defaultDynFlags fakeSettings fakeLlvmConfig) {
            ghcLink = NoLink
          , hscTarget = HscNothing
          , pkgDatabase = Just []
          , dirsToClean = dirs_to_clean
          , filesToClean = files_to_clean
          , nextTempSuffix = next_temp_suffix
          , thisInstalledUnitId = toInstalledUnitId (stringToUnitId "ghc-prim")
          }
  ; parsePragmasIntoDynFlags filename s baseFlags
  }
  where
    parsePragmasIntoDynFlags :: String -> String -> DynFlags -> IO DynFlags
    parsePragmasIntoDynFlags fp contents dflags0 = do
      let opts = getOptions dflags0 (stringToStringBuffer contents) fp
      (dflags, _, _) <- parseDynamicFilePragma dflags0 opts
      return dflags

fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])

fakeSettings :: Settings
fakeSettings = Settings
  { sTargetPlatform=platform
  , sPlatformConstants=platformConstants
  , sProjectVersion=cProjectVersion
  , sProgramName="ghc"
  , sOpt_P_fingerprint=fingerprint0
  , sTmpDir="."
  }
  where
    platform =
      Platform{
          platformWordSize=8
        , platformOS=OSUnknown
        , platformUnregisterised=True
        }
    -- This bit may need to be conditional on OS.
    platformConstants =
      PlatformConstants {
        pc_DYNAMIC_BY_DEFAULT=False
      , pc_WORD_SIZE=8
      , pc_STD_HDR_SIZE=1
      , pc_TAG_BITS=3
      , pc_BLOCKS_PER_MBLOCK=252
      , pc_BLOCK_SIZE=4096
      , pc_MIN_PAYLOAD_SIZE=1
      , pc_MAX_Real_Vanilla_REG=6
      , pc_MAX_Vanilla_REG=10
      , pc_MAX_Real_Long_REG=0
      }
