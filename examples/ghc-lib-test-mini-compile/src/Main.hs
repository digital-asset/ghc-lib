-- Copyright (c) 2019 - 2024, Digital Asset (Switzerland) GmbH and/or
-- its affiliates. All rights reserved. SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main (main) where

-- We use 0.x for HEAD
#if !MIN_VERSION_ghc_lib(1,0,0)
#  define GHC_9_16
#  include "ghc-lib-parser/ghc-9.16/Main.hs"
#  include "ghc-lib/ghc-9.16/Main.hs"
#elif MIN_VERSION_ghc_lib(9,14,0)
#  define GHC_9_14
#  include "ghc-lib-parser/ghc-9.14/Main.hs"
#  include "ghc-lib/ghc-9.14/Main.hs"
#elif MIN_VERSION_ghc_lib(9,12,0)
#  define GHC_9_12
#  include "ghc-lib-parser/ghc-9.12/Main.hs"
#  include "ghc-lib/ghc-9.12/Main.hs"
#elif MIN_VERSION_ghc_lib(9,10,0)
#  define GHC_9_10
#  include "ghc-lib-parser/ghc-9.10/Main.hs"
#  include "ghc-lib/ghc-9.10/Main.hs"
#elif MIN_VERSION_ghc_lib(9,8,0)
#  define GHC_9_8
#  include "ghc-lib-parser/ghc-9.8/Main.hs"
#  include "ghc-lib/ghc-9.8/Main.hs"
#elif MIN_VERSION_ghc_lib(9,6,0)
#  define GHC_9_6
#  include "ghc-lib-parser/ghc-9.6/Main.hs"
#  include "ghc-lib/ghc-9.6/Main.hs"
#elif MIN_VERSION_ghc_lib(9,4,0)
#  define GHC_9_4
#  include "ghc-lib-parser/ghc-9.4/Main.hs"
#  include "ghc-lib/ghc-9.4/Main.hs"
#elif MIN_VERSION_ghc_lib(9,2,0)
#  define GHC_9_2
#  include "ghc-lib-parser/ghc-9.2/Main.hs"
#  include "ghc-lib/ghc-9.2/Main.hs"
#elif MIN_VERSION_ghc_lib(9,0,0)
#  define GHC_9_0
#  include "ghc-lib-parser/ghc-9.0/Main.hs"
#  include "ghc-lib/ghc-9.0/Main.hs"
#elif MIN_VERSION_ghc_lib(8,10,0)
#  define GHC_8_10
#  include "ghc-lib-parser/ghc-8.10/Main.hs"
#  include "ghc-lib/ghc-8.10/Main.hs"
#else
#  define GHC_8_8
#  include "ghc-lib-parser/ghc-8.8/Main.hs"
#  include "ghc-lib/ghc-8.8/Main.hs"
#endif

import Paths_ghc_lib

import System.Environment
import System.Directory
import System.FilePath(takeDirectory)
import System.IO.Extra
import qualified Data.Map.Strict as Map
import Data.IORef

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      let dir = takeDirectory file
      s <- readFile' file
      flags <- mkDynFlags file s
      dataDir <- getDataDir
      createDirectoryIfMissing True $ dataDir ++ "/../mingw" -- hack: avoid "could not detect toolchain mingw"
      cm <- runGhc (Just dataDir) $ do
              let searchPaths = [dir]
              setSessionDynFlags flags { importPaths = searchPaths }
              compileToCoreSimplified file
      putStrLn $ showSDoc flags $ ppr cm
    _ -> fail "Exactly one file argument required"

ghclibPrimUnitId :: String
ghclibPrimUnitId =
#if defined (DAML_UNIT_IDS)
 "daml-prim"
#elif defined(GHC_9_12) || defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10) || defined (GHC_8_8)
 "ghc-prim"
#else
 "ghc-internal"
#endif

-- Create a DynFlags which is sufficiently filled in to work, but not
-- complete.
mkDynFlags :: String -> String -> IO DynFlags
mkDynFlags filename s = do

#if defined (GHC_8_8)

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
        , thisInstalledUnitId = toInstalledUnitId (stringToUnitId ghclibPrimUnitId)
        }
  parsePragmasIntoDynFlags filename s baseFlags
  where
    parsePragmasIntoDynFlags :: String -> String -> DynFlags -> IO DynFlags
    parsePragmasIntoDynFlags filepath contents dflags0 = do
      let opts = getOptions dflags0
                    (stringToStringBuffer contents) filepath
      (dflags, _, _) <- parseDynamicFilePragma dflags0 opts
      return dflags

#elif defined (GHC_8_10)

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
        , thisInstalledUnitId = toInstalledUnitId (stringToUnitId ghclibPrimUnitId)
        }
  parsePragmasIntoDynFlags filename s baseFlags
  where
    parsePragmasIntoDynFlags :: String -> String -> DynFlags -> IO DynFlags
    parsePragmasIntoDynFlags filepath contents dflags0 = do
      let opts = getOptions dflags0 (stringToStringBuffer contents) filepath
      (dflags, _, _) <- parseDynamicFilePragma dflags0 opts
      return dflags

#elif defined (GHC_9_0)

  dirs_to_clean <- newIORef Map.empty
  files_to_clean <- newIORef emptyFilesToClean
  next_temp_suffix <- newIORef 0

  let baseFlags =
        (defaultDynFlags fakeSettings fakeLlvmConfig) {
          ghcLink = NoLink
        , hscTarget = HscNothing
        , unitDatabases = Just []
        , dirsToClean = dirs_to_clean
        , filesToClean = files_to_clean
        , nextTempSuffix = next_temp_suffix
        , homeUnitId = toUnitId (stringToUnit ghclibPrimUnitId)
        }
  parsePragmasIntoDynFlags filename s baseFlags
  where
    parsePragmasIntoDynFlags :: String -> String -> DynFlags -> IO DynFlags
    parsePragmasIntoDynFlags filepath contents dflags0 = do
      let opts = getOptions dflags0
                    (stringToStringBuffer contents) filepath
      (dflags, _, _) <- parseDynamicFilePragma dflags0 opts
      return dflags

#elif defined (GHC_9_2)

  let baseFlags =
        (defaultDynFlags fakeSettings fakeLlvmConfig) {
          ghcLink = NoLink
        , backend = NoBackend
        , homeUnitId_ = toUnitId (stringToUnit ghclibPrimUnitId)
        }
  parsePragmasIntoDynFlags filename s baseFlags
  where
    parsePragmasIntoDynFlags :: String -> String -> DynFlags -> IO DynFlags
    parsePragmasIntoDynFlags filepath contents dflags0 = do
      let opts = getOptions dflags0
                    (stringToStringBuffer contents) filepath
      (dflags, _, _) <- parseDynamicFilePragma dflags0 opts
      return dflags

#elif defined (GHC_9_4)

  let baseFlags =
        (defaultDynFlags fakeSettings fakeLlvmConfig) {
          ghcLink = NoLink
        , backend = NoBackend
        , homeUnitId_ = toUnitId (stringToUnit ghclibPrimUnitId)
        }
  parsePragmasIntoDynFlags filename s baseFlags
  where
    parsePragmasIntoDynFlags :: String -> String -> DynFlags -> IO DynFlags
    parsePragmasIntoDynFlags filepath contents dflags0 = do
      let (_, opts) = getOptions (initParserOpts dflags0)
                        (stringToStringBuffer contents) filepath
      (dflags, _, _) <- parseDynamicFilePragma dflags0 opts
      return dflags

#elif (defined (GHC_9_6) || defined (GHC_9_8) || defined (GHC_9_10) || defined (GHC_9_12))

  let baseFlags =
        (defaultDynFlags fakeSettings) {
          ghcLink = NoLink
        , backend = noBackend
        , homeUnitId_ = toUnitId (stringToUnit ghclibPrimUnitId)
        }
  parsePragmasIntoDynFlags filename s baseFlags
  where
    parsePragmasIntoDynFlags :: String -> String -> DynFlags -> IO DynFlags
    parsePragmasIntoDynFlags filepath contents dflags0 = do
      let (_, opts) = getOptions (initParserOpts dflags0)
                        (stringToStringBuffer contents) filepath
      (dflags, _, _) <- parseDynamicFilePragma dflags0 opts
      return dflags

#else

  let baseFlags =
        (defaultDynFlags fakeSettings) {
          ghcLink = NoLink
        , backend = noBackend
        , homeUnitId_ = toUnitId (stringToUnit ghclibPrimUnitId)
        }
  parsePragmasIntoDynFlags filename s baseFlags
  where
    parsePragmasIntoDynFlags :: String -> String -> DynFlags -> IO DynFlags
    parsePragmasIntoDynFlags filepath contents dflags0 = do
      let (_, opts) = getOptions (initParserOpts dflags0)
#if (defined (GHC_9_16))
                        (initSourceErrorContext dflags0)
#endif
                        (supportedLanguagePragmas dflags0)
                        (stringToStringBuffer contents) filepath
      logger <- initLogger
      (dflags, _, _) <- parseDynamicFilePragma logger dflags0 opts
      return dflags

#endif

#if defined (GHC_8_8)

fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])

#elif defined (GHC_8_10) || defined (GHC_9_0) || defined (GHC_9_2) || defined (GHC_9_4)

fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []

#else
   {- defined (GHC_9_6) || defined (GHC_9_8) || defined (GHC_9_10) || defined (GHC_9_12) || defined (GHC_9_14) -}

#endif

fakeSettings :: Settings
fakeSettings = Settings {

#if defined (GHC_8_8)

    sTargetPlatform=platform
  , sPlatformConstants=platformConstants
  , sProjectVersion=cProjectVersion
  , sProgramName="ghc"
  , sOpt_P_fingerprint=fingerprint0
  , sTmpDir="."
  }
  where
    platform = Platform {
        platformWordSize=8
      , platformOS=OSUnknown
      , platformUnregisterised=True
      }

    platformConstants = PlatformConstants {
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

#elif defined (GHC_8_10)

    sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sToolSettings=toolSettings
  , sPlatformConstants=platformConstants
  }
  where
    fileSettings = FileSettings {
         fileSettings_tmpDir="."
       }

    toolSettings = ToolSettings {
         toolSettings_opt_P_fingerprint=fingerprint0
       }

    platformMisc = PlatformMisc {
        platformMisc_integerLibraryType=IntegerSimple
       }

    ghcNameVersion = GhcNameVersion{
         ghcNameVersion_programName="ghc"
       , ghcNameVersion_projectVersion=cProjectVersion
      }

    platform = Platform{
         platformWordSize=PW8
       , platformMini=PlatformMini {platformMini_arch=ArchUnknown, platformMini_os=OSUnknown}
       , platformUnregisterised=True
      }

    platformConstants = PlatformConstants {
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

#elif defined (GHC_9_0)

    sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sToolSettings=toolSettings
  , sPlatformConstants=platformConstants
  }
  where
    fileSettings = FileSettings {
         fileSettings_tmpDir="."
       , fileSettings_topDir="."
       , fileSettings_toolDir=Nothing
       , fileSettings_ghcUsagePath="."
       , fileSettings_ghciUsagePath="."
       , fileSettings_globalPackageDatabase="."
       }

    toolSettings = ToolSettings {
         toolSettings_opt_P_fingerprint=fingerprint0
       }

    platformMisc = PlatformMisc {
       }

    ghcNameVersion = GhcNameVersion {
         ghcNameVersion_programName="ghc"
       , ghcNameVersion_projectVersion=cProjectVersion
      }

    platform = Platform {
        platformByteOrder=LittleEndian
      , platformHasGnuNonexecStack=True
      , platformHasIdentDirective=False
      , platformHasSubsectionsViaSymbols=False
      , platformIsCrossCompiling=False
      , platformLeadingUnderscore=False
      , platformTablesNextToCode=False
      , platformWordSize=PW8
      , platformMini=PlatformMini {platformMini_arch=ArchUnknown, platformMini_os=OSUnknown}
      , platformUnregisterised=True
      }

    platformConstants = PlatformConstants {
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

#elif defined (GHC_9_2)

    sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sToolSettings=toolSettings
  }
  where
    fileSettings = FileSettings {
         fileSettings_tmpDir="."
       , fileSettings_topDir="."
       , fileSettings_toolDir=Nothing
       , fileSettings_ghcUsagePath="."
       , fileSettings_ghciUsagePath="."
       , fileSettings_globalPackageDatabase="."
       }

    toolSettings = ToolSettings {
         toolSettings_opt_P_fingerprint=fingerprint0
       }

    platformMisc = PlatformMisc {
       }

    ghcNameVersion = GhcNameVersion {
         ghcNameVersion_programName="ghc"
       , ghcNameVersion_projectVersion=cProjectVersion
      }

    platform =  genericPlatform

#elif (defined (GHC_9_4) || defined (GHC_9_6) || defined (GHC_9_8) || defined (GHC_9_10) || defined (GHC_9_12))
    sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sToolSettings=toolSettings
  }
  where
    fileSettings = FileSettings {
         fileSettings_topDir="."
       , fileSettings_toolDir=Nothing
       , fileSettings_ghcUsagePath="."
       , fileSettings_ghciUsagePath="."
       , fileSettings_globalPackageDatabase="."
       }

    toolSettings = ToolSettings {
         toolSettings_opt_P_fingerprint=fingerprint0
       }

    platformMisc = PlatformMisc {
       }

    ghcNameVersion = GhcNameVersion{
         ghcNameVersion_programName="ghc"
       , ghcNameVersion_projectVersion=cProjectVersion
      }

    platform =  genericPlatform

#elif (defined (GHC_9_4) || defined (GHC_9_6) || defined (GHC_9_8) || defined (GHC_9_10) || defined (GHC_9_12)) || defined (GHC_9_14)

    sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sToolSettings=toolSettings
  , sUnitSettings=unitSettings
  }
  where
    unitSettings = UnitSettings {
        unitSettings_baseUnitId = stringToUnitId "base"
      }

    fileSettings = FileSettings {
         fileSettings_topDir="."
       , fileSettings_toolDir=Nothing
       , fileSettings_ghcUsagePath="."
       , fileSettings_ghciUsagePath="."
       , fileSettings_globalPackageDatabase="."
       }

    toolSettings = ToolSettings {
         toolSettings_opt_P_fingerprint=fingerprint0
       , toolSettings_opt_JSP_fingerprint=fingerprint0
       , toolSettings_opt_CmmP_fingerprint=fingerprint0
       }

    platformMisc = PlatformMisc {
       }

    ghcNameVersion = GhcNameVersion{
         ghcNameVersion_programName="ghc"
       , ghcNameVersion_projectVersion=cProjectVersion
      }

    platform =  genericPlatform

#else
   {- defined (GHC_9_16) -}

    sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sToolSettings=toolSettings
  , sUnitSettings=unitSettings
  }
  where
    unitSettings = UnitSettings {
        unitSettings_baseUnitId = stringToUnitId "base"
      }

    fileSettings = FileSettings {
         fileSettings_topDir="."
       , fileSettings_toolDir=Nothing
       , fileSettings_ghcUsagePath="."
       , fileSettings_ghciUsagePath="."
       , fileSettings_globalPackageDatabase="."
       }

    toolSettings = ToolSettings {
         toolSettings_opt_P_fingerprint=fingerprint0
       , toolSettings_opt_JSP_fingerprint=fingerprint0
       , toolSettings_opt_CmmP_fingerprint=fingerprint0
       }

    platformMisc = PlatformMisc {
       }

    ghcNameVersion = GhcNameVersion{
         ghcNameVersion_programName="ghc"
       , ghcNameVersion_projectVersion=cProjectVersion
      }

    platform =  genericPlatform

#endif
