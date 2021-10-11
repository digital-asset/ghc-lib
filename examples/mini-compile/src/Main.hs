-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: (Apache-2.0 OR BSD-3-Clause)

{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main (main) where

-- We use 0.x for HEAD
#if !MIN_VERSION_ghc_lib(1,0,0)
#  define GHC_MASTER
#elif MIN_VERSION_ghc_lib(9,2,1)
#  define GHC_921
#elif MIN_VERSION_ghc_lib(9,0,0)
#  define GHC_901
#elif MIN_VERSION_ghc_lib(8,10,1)
#  define GHC_8101
#endif

import "ghc-lib" GHC
import "ghc-lib" Paths_ghc_lib
#if defined (GHC_MASTER) ||  defined (GHC_921) || defined (GHC_901)
#  if defined (GHC_MASTER)
import "ghc-lib-parser" GHC.Driver.Config.Parser
#  endif
import "ghc-lib-parser" GHC.Parser.Header
import "ghc-lib-parser" GHC.Unit.Module
import "ghc-lib-parser" GHC.Driver.Session
import "ghc-lib-parser" GHC.Data.StringBuffer
import "ghc-lib-parser" GHC.Utils.Fingerprint
import "ghc-lib-parser" GHC.Utils.Outputable
#  if !defined (GHC_901)
import "ghc-lib-parser" GHC.Driver.Ppr
#  endif
#else
import "ghc-lib-parser" HeaderInfo
import "ghc-lib-parser" Module
import "ghc-lib-parser" DynFlags
import "ghc-lib-parser" StringBuffer
import "ghc-lib-parser" Fingerprint
import "ghc-lib-parser" Outputable
#endif
#if defined (GHC_MASTER)  || defined (GHC_921) || defined (GHC_901)
import "ghc-lib-parser" GHC.Settings
import "ghc-lib-parser" GHC.Settings.Config
#elif defined (GHC_8101)
import "ghc-lib-parser" Config
import "ghc-lib-parser" ToolSettings
#endif
#if defined (GHC_MASTER) ||  defined (GHC_921) || defined (GHC_901) || defined (GHC_8101)
import "ghc-lib-parser" GHC.Platform
#else
import "ghc-lib-parser" Config
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
#if defined (GHC_MASTER) || defined (GHC_921)
       -- Intentionally empty (temp file flags have moved to HscEnv).
#else
  files_to_clean <- newIORef emptyFilesToClean
#endif
  next_temp_suffix <- newIORef 0
  let baseFlags =
        (defaultDynFlags fakeSettings fakeLlvmConfig) {
          ghcLink = NoLink
#if defined (GHC_MASTER) || defined (GHC_921)
        , backend = NoBackend
#else
        , hscTarget = HscNothing
#endif
#if defined (GHC_MASTER) || defined (GHC_921)
        -- Intentionally empty (unit related flags have moved to
        -- HscEnv).
#elif defined (GHC_901)
        , unitDatabases = Just []
#else
        , pkgDatabase = Just []
#endif
#if defined (GHC_MASTER) || defined (GHC_921)
       -- Intentionally empty (temp file flags have moved to HscEnv).
#else
        , dirsToClean = dirs_to_clean
        , filesToClean = files_to_clean
        , nextTempSuffix = next_temp_suffix
#endif
#if defined(DAML_UNIT_IDS)
        , thisInstalledUnitId = toInstalledUnitId (stringToUnitId "daml-prim")
#else
#if defined (GHC_MASTER) || defined (GHC_921)
        , homeUnitId_ = toUnitId (stringToUnit "ghc-prim")
#elif defined (GHC_901)
        , homeUnitId = toUnitId (stringToUnit "ghc-prim")
#else
        , thisInstalledUnitId = toInstalledUnitId (stringToUnitId "ghc-prim")
#endif
#endif
        }
  parsePragmasIntoDynFlags filename s baseFlags
  where
    parsePragmasIntoDynFlags :: String -> String -> DynFlags -> IO DynFlags
    parsePragmasIntoDynFlags fp contents dflags0 = do
      let opts = getOptions
#if defined (GHC_MASTER)
                 (initParserOpts dflags0)
#else
                 dflags0
#endif
                 (stringToStringBuffer contents) fp
      (dflags, _, _) <- parseDynamicFilePragma dflags0 opts
      return dflags

#if defined (GHC_MASTER)  || defined (GHC_921) || defined (GHC_901) || defined (GHC_8101)
fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []
#else
fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])
#endif

fakeSettings :: Settings
fakeSettings = Settings
#if defined (GHC_MASTER) ||  defined (GHC_921) || defined (GHC_901) || defined (GHC_8101)
  { sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
#  if !defined (GHC_MASTER) && !defined (GHC_921)
  , sPlatformConstants=platformConstants
#  endif
  , sToolSettings=toolSettings
  }
#else
  { sTargetPlatform=platform
  , sPlatformConstants=platformConstants
  , sProjectVersion=cProjectVersion
  , sProgramName="ghc"
  , sOpt_P_fingerprint=fingerprint0
  , sTmpDir="."
  }
#endif
  where
#if defined (GHC_MASTER)  || defined (GHC_921) || defined (GHC_901) || defined (GHC_8101)
    fileSettings = FileSettings {
#if defined (GHC_8101)
        fileSettings_tmpDir="."
#else
#if !defined(GHC_MASTER)
        fileSettings_tmpDir=".",
#endif
       fileSettings_topDir=".",
       fileSettings_toolDir=Nothing,
       fileSettings_ghcUsagePath=".",
       fileSettings_ghciUsagePath=".",
       fileSettings_globalPackageDatabase="."
#endif
      }

    toolSettings = ToolSettings {
        toolSettings_opt_P_fingerprint=fingerprint0
      }
    platformMisc = PlatformMisc {
#if !defined (GHC_MASTER)  && !defined (GHC_921) && !defined (GHC_901)
        platformMisc_integerLibraryType=IntegerSimple
#endif
      }
    ghcNameVersion =
      GhcNameVersion{
        ghcNameVersion_programName="ghc"
      , ghcNameVersion_projectVersion=cProjectVersion
      }
#endif
    platform =
#if defined (GHC_MASTER)
      genericPlatform
#else
      Platform{
#if defined (GHC_921) || defined (GHC_901)
      -- It doesn't matter what values we write here as these fields are
      -- not referenced for our purposes. However the fields are strict
      -- so we must say something.
        platformByteOrder=LittleEndian
      , platformHasGnuNonexecStack=True
      , platformHasIdentDirective=False
      , platformHasSubsectionsViaSymbols=False
      , platformIsCrossCompiling=False
      , platformLeadingUnderscore=False
      , platformTablesNextToCode=False
#if defined(GHC_921)
      , platform_constants = Nothing
#endif
#if !defined(GHC_921) && !defined(GHC_901)
      , platformConstants=platformConstants
#endif
      ,
#endif
#if defined (GHC_921)
        platformWordSize=PW8
      , platformArchOS=ArchOS {archOS_arch=ArchUnknown, archOS_OS=OSUnknown}
#elif defined (GHC_8101) || defined (GHC_901)
        platformWordSize=PW8
      , platformMini=PlatformMini {platformMini_arch=ArchUnknown, platformMini_os=OSUnknown}
#else
        platformWordSize=8
      , platformOS=OSUnknown
#endif
      , platformUnregisterised=True
      }
#endif
#if !defined (GHC_MASTER) && !defined (GHC_921)
    platformConstants =
       PlatformConstants {
         pc_DYNAMIC_BY_DEFAULT=False
       ,
         pc_WORD_SIZE=8
       , pc_STD_HDR_SIZE=1
       , pc_TAG_BITS=3
       , pc_BLOCKS_PER_MBLOCK=252
       , pc_BLOCK_SIZE=4096
       , pc_MIN_PAYLOAD_SIZE=1
       , pc_MAX_Real_Vanilla_REG=6
       , pc_MAX_Vanilla_REG=10
       , pc_MAX_Real_Long_REG=0
       }
#endif
