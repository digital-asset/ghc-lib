-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its
-- affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main (main) where

-- We use 0.x for HEAD
#if !MIN_VERSION_ghc_lib_parser(1,0,0)
#  define GHC_MASTER
#elif MIN_VERSION_ghc_lib_parser(9,2,0)
#  define GHC_921
#elif MIN_VERSION_ghc_lib_parser(9,0,0)
#  define GHC_901
#elif MIN_VERSION_ghc_lib_parser(8,10,0)
#  define GHC_8101
#endif

#if defined (GHC_MASTER)
import "ghc-lib-parser" GHC.Driver.Errors.Types
import "ghc-lib-parser" GHC.Types.Error hiding (getMessages)
import qualified "ghc-lib-parser" GHC.Types.Error (getMessages)
#endif
#if defined (GHC_MASTER)
import "ghc-lib-parser" GHC.Driver.Config.Parser
#endif
#if defined (GHC_MASTER) || defined (GHC_921)
import "ghc-lib-parser" GHC.Driver.Config
import "ghc-lib-parser" GHC.Utils.Logger
#endif
#if defined (GHC_MASTER) || defined (GHC_921) || defined (GHC_901) || defined (GHC_8101)
import "ghc-lib-parser" GHC.Hs
#else
import "ghc-lib-parser" HsSyn
#endif
#if defined (GHC_MASTER) || defined (GHC_921) || defined (GHC_901)
import "ghc-lib-parser" GHC.Settings.Config
import "ghc-lib-parser" GHC.Driver.Session
import "ghc-lib-parser" GHC.Data.StringBuffer
import "ghc-lib-parser" GHC.Utils.Fingerprint
import "ghc-lib-parser" GHC.Parser.Lexer
import "ghc-lib-parser" GHC.Types.Name.Reader
import "ghc-lib-parser" GHC.Utils.Error
import qualified "ghc-lib-parser" GHC.Parser
import "ghc-lib-parser" GHC.Data.FastString
import "ghc-lib-parser" GHC.Utils.Outputable
#if !defined (GHC_901)
import "ghc-lib-parser" GHC.Driver.Ppr
import "ghc-lib-parser" GHC.Parser.Errors.Ppr
#endif
import "ghc-lib-parser" GHC.Types.SrcLoc
import "ghc-lib-parser" GHC.Utils.Panic
#  if defined (GHC_MASTER) || defined (GHC_921)
import "ghc-lib-parser" GHC.Types.SourceError
#  else
import "ghc-lib-parser" GHC.Driver.Types
#  endif
import "ghc-lib-parser" GHC.Parser.Header
import "ghc-lib-parser" GHC.Parser.Annotation
#else
import "ghc-lib-parser" Config
import "ghc-lib-parser" DynFlags
import "ghc-lib-parser" StringBuffer
import "ghc-lib-parser" Fingerprint
import "ghc-lib-parser" Lexer
import "ghc-lib-parser" RdrName
import "ghc-lib-parser" ErrUtils
import qualified "ghc-lib-parser" Parser
import "ghc-lib-parser" FastString
import "ghc-lib-parser" Outputable
import "ghc-lib-parser" SrcLoc
import "ghc-lib-parser" Panic
import "ghc-lib-parser" HscTypes
import "ghc-lib-parser" HeaderInfo
import "ghc-lib-parser" ApiAnnotation
#endif
#if defined (GHC_MASTER) || defined (GHC_921) || defined (GHC_901)
import "ghc-lib-parser" GHC.Settings
#elif defined (GHC_8101)
import "ghc-lib-parser" ToolSettings
#endif
#if defined (GHC_MASTER) || defined (GHC_921) || defined (GHC_901) || defined (GHC_8101)
import "ghc-lib-parser" GHC.Platform
#else
import "ghc-lib-parser" Bag
import "ghc-lib-parser" Platform
#endif
#if defined (GHC_MASTER) || defined (GHC_921) || defined (GHC_901) || defined (GHC_8101)
import "ghc-lib-parser" GHC.Hs.Dump
#else
import "ghc-lib-parser" HsDumpAst
#endif

import Control.Monad.Extra
import System.Environment
import System.IO.Extra
import qualified Data.Map as Map
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data

fakeSettings :: Settings
fakeSettings = Settings
#if defined (GHC_MASTER) || defined (GHC_921) || defined (GHC_901) || defined (GHC_8101)
  { sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
#  if !defined (GHC_MASTER)
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
  }
#endif
  where

#if defined (GHC_MASTER) || defined (GHC_921) || defined (GHC_901) || defined (GHC_8101)
    toolSettings = ToolSettings {
      toolSettings_opt_P_fingerprint=fingerprint0
      }
    fileSettings = FileSettings {}
    platformMisc = PlatformMisc {}
    ghcNameVersion =
      GhcNameVersion{ghcNameVersion_programName="ghc"
                    ,ghcNameVersion_projectVersion=cProjectVersion
                    }
#endif
    platform =
      Platform {
#if defined (GHC_MASTER) || defined (GHC_921) || defined (GHC_901)
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
#if defined(GHC_MASTER)
      , platform_constants = Nothing
#endif
#if !defined(GHC_MASTER) && !defined(GHC_901)
      , platformConstants=platformConstants
#endif
      ,
#endif
#if defined (GHC_MASTER) || defined (GHC_921)
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
#if !defined (GHC_MASTER)
    platformConstants =
      PlatformConstants{
          pc_DYNAMIC_BY_DEFAULT=False
        ,
          pc_WORD_SIZE=8
    }
#endif

#if defined (GHC_MASTER) || defined (GHC_921) || defined (GHC_901) || defined (GHC_8101)
fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []
#else
fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])
#endif

#if defined (GHC_MASTER) || defined (GHC_921) || defined (GHC_901)
parse :: String -> DynFlags -> String -> ParseResult (Located HsModule)
#else
parse :: String -> DynFlags -> String -> ParseResult (Located (HsModule GhcPs))
#endif
parse filename flags str =
#if defined (GHC_MASTER) || defined (GHC_921) || defined (GHC_901)
  unP GHC.Parser.parseModule parseState
#else
  unP Parser.parseModule parseState
#endif
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState =
#if defined (GHC_MASTER) || defined (GHC_921)
      initParserState (initParserOpts flags) buffer location
#else
      mkPState flags buffer location
#endif

parsePragmasIntoDynFlags :: DynFlags -> FilePath -> String -> IO (Maybe DynFlags)
parsePragmasIntoDynFlags flags filepath str =
  catchErrors $ do
    let opts = getOptions
#if defined (GHC_MASTER)
                 (initParserOpts flags)
#else
                 flags
#endif
                 (stringToStringBuffer str) filepath
    (flags, _, _) <- parseDynamicFilePragma flags opts
    return $ Just flags
  where
    catchErrors :: IO (Maybe DynFlags) -> IO (Maybe DynFlags)
    catchErrors act = handleGhcException reportErr
                        (handleSourceError reportErr act)
    reportErr e = do putStrLn $ "error : " ++ show e; return Nothing

#if defined (GHC_MASTER) || defined (GHC_921) || defined (GHC_901)
dumpParseTree :: DynFlags -> Located HsModule -> IO ()
#else
dumpParseTree :: DynFlags -> Located (HsModule GhcPs) -> IO ()
#endif
dumpParseTree flags m =
#if defined(GHC_MASTER) || defined (GHC_921)
  do
    logger <- initLogger
#  if defined (GHC_MASTER)
    logDumpMsg logger "" $ showAstData NoBlankSrcSpan NoBlankEpAnnotations m
#  else
    putDumpMsg logger flags (mkDumpStyle alwaysQualify) Opt_D_dump_parsed_ast "" FormatText $ showAstData NoBlankSrcSpan NoBlankEpAnnotations m
#  endif
#elif defined(GHC_901)
  dumpAction flags (mkDumpStyle alwaysQualify) (dumpOptionsFromFlag Opt_D_dump_parsed_ast) "" FormatText $ showAstData NoBlankSrcSpan m
#else
  dumpSDoc flags alwaysQualify Opt_D_dump_parsed_ast "" $ showAstData NoBlankSrcSpan m
#endif

#if defined (GHC_MASTER) || defined (GHC_921) || defined (GHC_901)
stripLocs :: Located HsModule -> Located HsModule
#else
stripLocs :: Located (HsModule GhcPs) -> Located (HsModule GhcPs)
#endif
stripLocs = transformBi $ const noSrcSpan

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      s <- readFile' file
      flags <-
        parsePragmasIntoDynFlags
          (defaultDynFlags fakeSettings fakeLlvmConfig) file s
      whenJust flags $ \flags ->
         case parse file (flags `gopt_set` Opt_KeepRawTokenStream) s of
#if defined (GHC_MASTER)
            PFailed s -> report flags $ GHC.Types.Error.getMessages (GhcPsMessage <$> snd (getMessages s))
#elif defined (GHC_921)
            PFailed s -> report flags $ fmap pprError (snd (getMessages s))
#elif defined (GHC_901) || defined (GHC_8101)
            PFailed s -> report flags $ snd (getMessages s flags)
#else
            PFailed _ loc err -> report flags $ unitBag $ mkPlainErrMsg flags loc err
#endif
            POk s m -> do
#if defined (GHC_MASTER)
              let (wrns, errs) = getMessages s
              report flags $ GHC.Types.Error.getMessages (GhcPsMessage <$> wrns)
              report flags $ GHC.Types.Error.getMessages (GhcPsMessage <$> errs)
#elif defined (GHC_921)
              let (wrns, errs) = getMessages s
              report flags (fmap pprWarning wrns)
              report flags (fmap pprError errs)
#else
              let (wrns, errs) = getMessages s flags
              report flags wrns
              report flags errs
#endif
              when (null errs) $ dumpParseTree flags (stripLocs m)
    _ -> fail "Exactly one file argument required"
  where
    report flags msgs =
      sequence_
        [ putStrLn $ showSDoc flags msg
        | msg <-
#if defined (GHC_MASTER) || defined (GHC_921)
                  pprMsgEnvelopeBagWithLoc msgs
#else
                  pprErrMsgBagWithLoc msgs
#endif
        ]
