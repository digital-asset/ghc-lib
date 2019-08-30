-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: (Apache-2.0 OR BSD-3-Clause)

{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main (main) where

-- We use 0.x for HEAD
#if !MIN_VERSION_ghc_lib(1,0,0)
#  define GHC_MASTER
#endif

import "ghc-lib" GHC
import "ghc-lib" Paths_ghc_lib
import "ghc-lib-parser" HeaderInfo
import "ghc-lib-parser" Module
import "ghc-lib-parser" Config
import "ghc-lib-parser" DynFlags
import "ghc-lib-parser" StringBuffer
import "ghc-lib-parser" Fingerprint
import "ghc-lib-parser" Outputable

#ifdef GHC_MASTER
import "ghc-lib-parser" GHC.Platform
import "ghc-lib-parser" ToolSettings
#else
import "ghc-lib-parser" Platform
#endif

import System.Environment
import System.Directory
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
      -- On mingw we avoid a "could not detect toolchain mingw"
      -- runtime error. The error originates from `findToolDir`
      -- indirectly invoked by `initGhcMonad` from `runGhc`). This
      -- line tricks `findToolDir`.
      createDirectoryIfMissing True $ dataDir ++ "/../mingw"
      cm <- runGhc (Just dataDir) $ do
              setSessionDynFlags flags
              compileToCoreSimplified file
      putStrLn $ showSDoc flags $ ppr cm
    _ -> fail "Exactly one file argument required"


-- | Create a DynFlags which is sufficiently filled in to work, but not complete
mkDynFlags :: String -> String -> IO DynFlags
mkDynFlags filename s = do
  dirs_to_clean <- newIORef Map.empty
  files_to_clean <- newIORef emptyFilesToClean
  next_temp_suffix <- newIORef 0
  let baseFlags =
        (defaultDynFlags fakeSettings fakeLlvmConfig) {
          ghcLink = NoLink
        , hscTarget = HscNothing
        , pkgDatabase = Just []
        , dirsToClean = dirs_to_clean
        , filesToClean = files_to_clean
        , nextTempSuffix = next_temp_suffix
#ifdef DAML_UNIT_IDS
        , thisInstalledUnitId = toInstalledUnitId (stringToUnitId "daml-prim")
#else
        , thisInstalledUnitId = toInstalledUnitId (stringToUnitId "ghc-prim")
#endif
        }
  parsePragmasIntoDynFlags filename s baseFlags
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
#ifndef GHC_MASTER
  { sTargetPlatform=platform
  , sPlatformConstants=platformConstants
  , sProjectVersion=cProjectVersion
  , sProgramName="ghc"
  , sOpt_P_fingerprint=fingerprint0
  , sTmpDir="."
  }
#else
  { sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sPlatformConstants=platformConstants
  , sToolSettings=toolSettings
  }
#endif
  where
#ifdef GHC_MASTER
    fileSettings = FileSettings {
        fileSettings_tmpDir="."
      }
    toolSettings = ToolSettings {
        toolSettings_opt_P_fingerprint=fingerprint0
      }
    platformMisc = PlatformMisc {
        platformMisc_integerLibraryType=IntegerSimple
      }
    ghcNameVersion =
      GhcNameVersion{
        ghcNameVersion_programName="ghc"
      , ghcNameVersion_projectVersion=cProjectVersion
      }
#endif
    platform =
      Platform{
#ifdef GHC_MASTER
        platformWordSize=PW8
      , platformArch=ArchUnknown
#else
        platformWordSize=8
#endif
      , platformOS=OSUnknown
      , platformUnregisterised=True
      }
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
