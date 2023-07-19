-- Copyright (c) 2019 - 2023, Digital Asset (Switzerland) GmbH and/or
-- its affiliates. All rights reserved. SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

-- hlint examples/ghc-lib-test-mini-hlint/src --cpp-include examples/ghc-lib-test-mini-hlint/extra-source-files

{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main (main) where

-- We use 0.x for HEAD
#if !MIN_VERSION_ghc_lib_parser(1,0,0)
#  define GHC_9_10
#  include "ghc-9.10/Main.hs"
#elif MIN_VERSION_ghc_lib_parser(9,8,0)
#  define GHC_9_8
#  include "ghc-9.8/Main.hs"
#elif MIN_VERSION_ghc_lib_parser(9,6,0)
#  define GHC_9_6
#  include "ghc-9.6/Main.hs"
#elif MIN_VERSION_ghc_lib_parser(9,4,0)
#  define GHC_9_4
#  include "ghc-9.4/Main.hs"
#elif MIN_VERSION_ghc_lib_parser(9,2,0)
#  define GHC_9_2
#  include "ghc-9.2/Main.hs"
#elif MIN_VERSION_ghc_lib_parser(9,0,0)
#  define GHC_9_0
#  include "ghc-9.0/Main.hs"
#elif MIN_VERSION_ghc_lib_parser(8,10,0)
#  define GHC_8_10
#  include "ghc-8.10/Main.hs"
#else
#  define GHC_8_8
#  include "ghc-8.8/Main.hs"
#endif

import Control.Monad.Extra
import System.Environment
import System.IO.Extra
import qualified Data.Map as Map
import Data.Generics.Uniplate.Operations
import Data.Generics.Uniplate.Data

fakeSettings :: Settings
fakeSettings = Settings
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10)
  { sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
#if !(defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2))
  , sPlatformConstants=platformConstants
#endif
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
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10)
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
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2)
      genericPlatform
#else
      Platform{
#if defined (GHC_9_0)
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
      ,
#endif
#if defined (GHC_8_10) || defined (GHC_9_0)
        platformWordSize=PW8
      , platformMini=PlatformMini {platformMini_arch=ArchUnknown, platformMini_os=OSUnknown}
#else
        platformWordSize=8
      , platformOS=OSUnknown
#endif
      , platformUnregisterised=True
      }
#endif
#if !defined (GHC_9_10) && !defined(GHC_9_8) && !defined (GHC_9_6) && !defined(GHC_9_4) && !defined(GHC_9_2)
    platformConstants = PlatformConstants{ pc_DYNAMIC_BY_DEFAULT=False, pc_WORD_SIZE=8 }
#endif

#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6)
-- Intentionally empty
#elif defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0) || defined (GHC_8_10)
fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []
#else
fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])
#endif

#if defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0)
parse :: String -> DynFlags -> String -> ParseResult (Located HsModule)
#else
parse :: String -> DynFlags -> String -> ParseResult (Located (HsModule GhcPs))
#endif
parse filename flags str =
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2) || defined (GHC_9_0)
  unP GHC.Parser.parseModule parseState
#else
  unP Parser.parseModule parseState
#endif
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState =
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2)
      initParserState (initParserOpts flags) buffer location
#else
      mkPState flags buffer location
#endif

parsePragmasIntoDynFlags :: DynFlags -> FilePath -> String -> IO (Maybe DynFlags)
parsePragmasIntoDynFlags flags filepath str =
  catchErrors $ do
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) ||  defined (GHC_9_4)
    let (_, opts) = getOptions (initParserOpts flags)
                      (stringToStringBuffer str) filepath
#else
    let opts = getOptions flags
                      (stringToStringBuffer str) filepath
#endif
    (flags, _, _) <- parseDynamicFilePragma flags opts
    return $ Just flags
  where
    catchErrors :: IO (Maybe DynFlags) -> IO (Maybe DynFlags)
    catchErrors act = handleGhcException reportGhcException
                        (handleSourceError reportSourceErr act)

    reportGhcException e = do
      print e; return Nothing

    reportSourceErr msgs = do
      putStrLn $ head
             [ showSDoc flags msg
             | msg <-
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6)
                      pprMsgEnvelopeBagWithLocDefault . getMessages
#elif defined (GHC_9_4)
                      pprMsgEnvelopeBagWithLoc . getMessages
#elif defined (GHC_9_2)
                      pprMsgEnvelopeBagWithLoc
#else
                      pprErrMsgBagWithLoc
#endif
                      $ srcErrorMessages msgs
             ]
      return Nothing

idNot :: RdrName
idNot = mkVarUnqual (fsLit "not")

isNegated :: HsExpr GhcPs -> Bool
isNegated (HsApp _ (L _ (HsVar _ (L _ id))) _) = id == idNot
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4)
isNegated (HsPar _ _ (L _ e) _) = isNegated e
#else
isNegated (HsPar _ (L _ e)) = isNegated e
#endif
isNegated _ = False

#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2)
analyzeExpr :: DynFlags -> LocatedA (HsExpr GhcPs) -> IO ()
#else
analyzeExpr :: DynFlags -> Located (HsExpr GhcPs) -> IO ()
#endif
analyzeExpr flags (L loc expr) = do
  case expr of
    HsApp _ (L _ (HsVar _ (L _ id))) (L _ e)
        | id == idNot, isNegated e ->
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2)
            putStrLn (showSDoc flags (ppr (locA loc))
#else
            putStrLn (showSDoc flags (ppr loc)
#endif
                      ++ " : lint : double negation "
                      ++ "`" ++ showSDoc flags (ppr expr) ++ "'")
    _ -> return ()

#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6)
analyzeModule :: DynFlags -> Located (HsModule GhcPs) -> IO ()
#elif defined (GHC_9_4) || defined (GHC_9_2)
analyzeModule :: DynFlags -> Located HsModule -> IO ()
#elif defined (GHC_9_0)
analyzeModule :: DynFlags -> Located HsModule -> ApiAnns -> IO ()
#else
analyzeModule :: DynFlags -> Located (HsModule GhcPs) -> ApiAnns -> IO ()
#endif
analyzeModule flags (L _ modu)
#if !(defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4) || defined (GHC_9_2))
                                _ -- ApiAnns
#endif
 = sequence_ [analyzeExpr flags e | e <- universeBi modu]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      s <- readFile' file
      flags <-
        parsePragmasIntoDynFlags
#if defined (GHC_9_10) || defined(GHC_9_8) || defined (GHC_9_6)
          (defaultDynFlags fakeSettings) file s
#else
          (defaultDynFlags fakeSettings fakeLlvmConfig) file s
#endif
      whenJust flags $ \flags ->
         case parse file (flags `gopt_set` Opt_KeepRawTokenStream)s of
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4)
            PFailed s -> report flags $ GhcPsMessage <$> snd (getPsMessages s)
#elif defined (GHC_9_2)
            PFailed s -> report flags $ fmap pprError (snd (getMessages s))
#elif defined (GHC_9_0) || defined (GHC_8_10)
            PFailed s -> report flags $ snd (getMessages s flags)
#else
            PFailed _ loc err -> report flags $ unitBag $ mkPlainErrMsg flags loc err
#endif
            POk s m -> do
#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined (GHC_9_4)
              let (wrns, errs) = getPsMessages s
              report flags $ GhcPsMessage <$> wrns
              report flags $ GhcPsMessage <$> errs
#elif defined (GHC_9_2)
              let (wrns, errs) = getMessages s
              report flags (fmap pprWarning wrns)
              report flags (fmap pprError errs)
#else
              let (wrns, errs) = getMessages s flags
              report flags wrns
              report flags errs
#endif
              when (null errs) $
                analyzeModule flags m
#if !(defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6) || defined(GHC_9_4) || defined (GHC_9_2))
                                      (harvestAnns s)
#endif
    _ -> fail "Exactly one file argument required"
  where

#if defined (GHC_9_10) || defined (GHC_9_8) || defined (GHC_9_6)
    report flags msgs = do
      let opts = initDiagOpts flags
          print_config = initPrintConfig flags
      logger <- initLogger
      printMessages logger print_config opts msgs
#elif defined (GHC_9_4)
    report flags msgs = do
      logger <- initLogger
      let opts = initDiagOpts flags
      printMessages logger opts msgs
#else
    report flags msgs =
      sequence_
        [ putStrLn $ showSDoc flags msg
        | msg <-
#  if defined (GHC_9_2)
                  pprMsgEnvelopeBagWithLoc msgs
#  else
                  pprErrMsgBagWithLoc msgs
#  endif
        ]
#endif
#if !(defined (GHC_9_10) || defined(GHC_9_8) || defined (GHC_9_6) || defined(GHC_9_4) || defined (GHC_9_2))
    harvestAnns pst =
#  if defined (GHC_9_0)
        ApiAnns {
              apiAnnItems = Map.fromListWith (++) $ annotations pst
            , apiAnnEofPos = Nothing
            , apiAnnComments = Map.fromListWith (++) $ annotations_comments pst
            , apiAnnRogueComments = comment_q pst
            }
#  else
      ( Map.fromListWith (++) $ annotations pst
      , Map.fromList ((noSrcSpan, comment_q pst) : annotations_comments pst)
      )
#  endif
#endif
