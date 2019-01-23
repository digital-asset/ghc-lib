{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main (main) where

import "ghc-lib" GHC
import "ghc-lib" Paths_ghc_lib
import "ghc-lib" BasicTypes
import "ghc-lib" DriverPhases
import "ghc-lib" GHC.LanguageExtensions.Type
import "ghc-lib" HeaderInfo
import "ghc-lib" Module
import "ghc-lib" Config
import "ghc-lib" DynFlags
import "ghc-lib" Platform
import "ghc-lib" StringBuffer
import "ghc-lib" Fingerprint
import "ghc-lib" ErrUtils
import "ghc-lib" Lexer
import qualified "ghc-lib" Parser
import FastString
import Outputable
import SrcLoc
import System.Environment
import System.IO.Extra
import qualified Data.Map.Strict as Map
import Data.Time.Clock
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import System.FilePath.Posix
import System.Posix.Types(EpochTime)
import System.Posix.Files

fakeSettings :: Settings
fakeSettings = Settings
  { sTargetPlatform=platform
  , sPlatformConstants=platformConstants
  , sProjectVersion=cProjectVersion
  , sProgramName="ghc"
  , sOpt_P_fingerprint=fingerprint0
  }
  where
    platform =
      Platform{platformWordSize=8
              , platformOS=OSUnknown
              , platformUnregisterised=True}
    platformConstants =
      PlatformConstants{pc_DYNAMIC_BY_DEFAULT=False,pc_WORD_SIZE=8}

fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])

parse :: String -> DynFlags -> String -> ParseResult (Located (HsModule GhcPs))
parse filename flags str =
  unP Parser.parseModule parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState flags buffer location

parsePragmasIntoDynFlags :: String -> String -> DynFlags -> IO DynFlags
parsePragmasIntoDynFlags fp contents dflags0 = do
    let opts = getOptions dflags0 (stringToStringBuffer contents) fp
    (dflags, _, _) <- parseDynamicFilePragma dflags0 opts
    return dflags

mkModSummary
  :: String  -- file name
  -> UTCTime -- file modification time
  -> DynFlags -- dyn flags
  -> String  -- file contents
  -> Located (HsModule GhcPs)
  -> ModSummary
mkModSummary filename filetime flags str (L loc rdr_module) = do
  let mod_name = maybe (GHC.mkModuleName "Main") unLoc $ hsmodName rdr_module
  let implicit_prelude = xopt ImplicitPrelude flags
      implicit_imports = mkPrelImports mod_name loc implicit_prelude $ hsmodImports rdr_module
      mod_loc =
        ModLocation {
            ml_hs_file = Just filename
          , ml_hi_file = replaceExtension filename "hi"
          , ml_obj_file = replaceExtension filename "o"
          }
      imports = [(fmap sl_fs $ ideclPkgQual i, ideclName i)
                | i <- map GHC.unLoc $ implicit_imports ++ GHC.hsmodImports rdr_module
                ]
      InstalledUnitId unit_id = thisInstalledUnitId flags
    in
    ModSummary {
      ms_mod = mkModule (fsToUnitId unit_id) mod_name
    , ms_location = mod_loc
    , ms_hs_date = filetime
    , ms_textual_imps = imports
    , ms_hspp_file = filename
    , ms_hspp_opts = flags
    , ms_hspp_buf = Just (stringToStringBuffer str)
    -- defaults:
    , ms_hsc_src = HsSrcFile
    , ms_obj_date = Nothing
    , ms_iface_date = Nothing
    , ms_srcimps = []        -- source imports are not allowed
    , ms_parsed_mod = Nothing
    }

epochToUTC :: EpochTime -> UTCTime
epochToUTC = posixSecondsToUTCTime . realToFrac

setThisInstalledUnitId :: UnitId -> DynFlags -> DynFlags
setThisInstalledUnitId unitId dflags =
  dflags {thisInstalledUnitId = toInstalledUnitId unitId}

main :: IO ()
main = do
  [filename] <- getArgs
  s <- readFile' filename
  flags <- parsePragmasIntoDynFlags
             filename
             s
             (setThisInstalledUnitId
               (stringToUnitId "ghc-prim")
               (defaultDynFlags fakeSettings fakeLlvmConfig)
             )
  case parse filename flags s of
    PFailed _ loc err ->
      putStrLn (showSDoc flags (pprLocErrMsg (mkPlainErrMsg flags loc err)))
    POk pst rdr_module ->
      let hpm_annotations =
            (Map.fromListWith (++) $ annotations pst,
              Map.fromList ((noSrcSpan,comment_q pst)
                             :annotations_comments pst))
      in do
        status <- getFileStatus filename
        let mtime = epochToUTC (modificationTime status)
        let ms = mkModSummary filename mtime flags s rdr_module
        let pm = ParsedModule {
                pm_mod_summary = ms
              , pm_parsed_source = rdr_module
              , pm_extra_src_files=[] -- src imports not allowed
              , pm_annotations = hpm_annotations
              }
        dataDir <- getDataDir
        tcm <- runGhc (Just dataDir) (typecheckModule pm >>= \tcm -> return tcm)
        return ()
