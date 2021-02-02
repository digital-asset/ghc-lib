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
#elif MIN_VERSION_ghc_lib_parser(9,0,0)
#  define GHC_901
#elif MIN_VERSION_ghc_lib_parser(8,10,1)
#  define GHC_8101
#endif

#if defined (GHC_MASTER)
import "ghc-lib-parser" GHC.Driver.Config
#endif
#if defined (GHC_MASTER) || defined (GHC_901) || defined (GHC_8101)
import "ghc-lib-parser" GHC.Hs
#else
import "ghc-lib-parser" HsSyn
#endif
#if defined (GHC_MASTER) || defined (GHC_901)
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
#  if !defined (GHC_901)
import "ghc-lib-parser" GHC.Driver.Ppr
import "ghc-lib-parser" GHC.Parser.Errors.Ppr
#  endif
import "ghc-lib-parser" GHC.Types.SrcLoc
import "ghc-lib-parser" GHC.Utils.Panic
#  if defined (GHC_MASTER)
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
#if defined (GHC_MASTER) || defined (GHC_901)
import "ghc-lib-parser" GHC.Settings
#elif defined (GHC_8101)
import "ghc-lib-parser" ToolSettings
#endif
#if defined (GHC_MASTER) || defined (GHC_901) || defined (GHC_8101)
import "ghc-lib-parser" GHC.Platform
#else
import "ghc-lib-parser" Bag
import "ghc-lib-parser" Platform
#endif

import Control.Monad.Extra
import System.Environment
import System.IO.Extra
import qualified Data.Map as Map
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data

fakeSettings :: Settings
fakeSettings = Settings
#if defined (GHC_MASTER) || defined (GHC_901) || defined (GHC_8101)
  { sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sPlatformConstants=platformConstants
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
#if defined (GHC_MASTER) || defined (GHC_901) || defined (GHC_8101)
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
      Platform{
#if defined (GHC_MASTER) || defined (GHC_901)
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
#if !defined (GHC_901)
      , platformConstants=platformConstants
#endif
      ,
#endif
#if defined (GHC_MASTER)
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
    platformConstants =
      PlatformConstants{pc_DYNAMIC_BY_DEFAULT=False,pc_WORD_SIZE=8}

#if defined (GHC_MASTER) || defined (GHC_901) || defined (GHC_8101)
fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []
#else
fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])
#endif

#if defined (GHC_MASTER) || defined (GHC_901)
parse :: String -> DynFlags -> String -> ParseResult (Located HsModule)
#else
parse :: String -> DynFlags -> String -> ParseResult (Located (HsModule GhcPs))
#endif
parse filename flags str =
#if defined (GHC_MASTER) || defined (GHC_901)
  unP GHC.Parser.parseModule parseState
#else
  unP Parser.parseModule parseState
#endif
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState =
#if defined (GHC_MASTER)
      initParserState (initParserOpts flags) buffer location
#else
      mkPState flags buffer location
#endif

parsePragmasIntoDynFlags :: DynFlags -> FilePath -> String -> IO (Maybe DynFlags)
parsePragmasIntoDynFlags flags filepath str =
  catchErrors $ do
    let opts = getOptions flags (stringToStringBuffer str) filepath
    (flags, _, _) <- parseDynamicFilePragma flags opts
    return $ Just flags
  where
    catchErrors :: IO (Maybe DynFlags) -> IO (Maybe DynFlags)
    catchErrors act = handleGhcException reportErr
                        (handleSourceError reportErr act)
    reportErr e = do putStrLn $ "error : " ++ show e; return Nothing

idNot :: RdrName
idNot = mkVarUnqual (fsLit "not")

isNegated :: HsExpr GhcPs -> Bool
isNegated (HsApp _ (L _ (HsVar _ (L _ id))) _) = id == idNot
isNegated (HsPar _ (L _ e)) = isNegated e
isNegated _ = False

analyzeExpr :: DynFlags -> Located (HsExpr GhcPs) -> IO ()
analyzeExpr flags (L loc expr) =
  case expr of
    HsApp _ (L _ (HsVar _ (L _ id))) (L _ e)
        | id == idNot, isNegated e ->
            putStrLn (showSDoc flags (ppr loc)
                      ++ " : lint : double negation "
                      ++ "`" ++ showSDoc flags (ppr expr) ++ "'")
    _ -> return ()

#if defined (GHC_MASTER) || defined (GHC_901)
analyzeModule :: DynFlags -> Located HsModule -> ApiAnns -> IO ()
#else
analyzeModule :: DynFlags -> Located (HsModule GhcPs) -> ApiAnns -> IO ()
#endif
analyzeModule flags (L _ modu) _ =
  sequence_ [analyzeExpr flags e | e <- universeBi modu]

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
         case parse file (flags `gopt_set` Opt_KeepRawTokenStream)s of
#if defined (GHC_MASTER)
            PFailed s -> report flags $ fmap pprError (snd (getMessages s))
#elif defined (GHC_901) || defined (GHC_8101)
            PFailed s -> report flags $ snd (getMessages s flags)
#else
            PFailed _ loc err -> report flags $ unitBag $ mkPlainErrMsg flags loc err
#endif
            POk s m -> do
#if defined (GHC_MASTER)
              let (wrns, errs) = getMessages s
              report flags (fmap pprWarning wrns)
              report flags (fmap pprError errs)
#else
              let (wrns, errs) = getMessages s flags
              report flags wrns
              report flags errs
#endif
              when (null errs) $
               analyzeModule flags m (harvestAnns s)
    _ -> fail "Exactly one file argument required"
  where

    report flags msgs =
      sequence_
        [ putStrLn $ showSDoc flags msg
        | msg <-
#if defined (GHC_MASTER)
                  pprMsgEnvelopeBagWithLoc msgs
#else
                  pprErrMsgBagWithLoc msgs
#endif
        ]
    harvestAnns pst =
#if defined (GHC_MASTER) || defined (GHC_901)
        ApiAnns {
              apiAnnItems = Map.fromListWith (++) $ annotations pst
            , apiAnnEofPos = Nothing
            , apiAnnComments = Map.fromListWith (++) $ annotations_comments pst
            , apiAnnRogueComments = comment_q pst
            }
#else
      ( Map.fromListWith (++) $ annotations pst
      , Map.fromList ((noSrcSpan, comment_q pst) : annotations_comments pst)
      )
#endif
