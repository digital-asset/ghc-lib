-- Copyright (c) 2019 - 2023, Digital Asset (Switzerland) GmbH and/or
-- its affiliates. All rights reserved. SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

-- hlint examples/ghc-lib-test-mini-hlint/src --cpp-include examples/ghc-lib-test-mini-hlint/extra-source-files

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
#if __GLASGOW_HASKELL__ >= 908
{-# OPTIONS_GHC -Wx-partial #-}
#endif

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

#if defined (GHC_8_10)
import Bag
#elif defined (GHC_9_0) || defined (GHC_9_2)
import GHC.Data.Bag
#endif

#if defined (GHC_8_8) || defined (GHC_8_10)

parse :: String -> DynFlags -> String -> ParseResult (Located (HsModule GhcPs))
parse filename flags str = unP Parser.parseModule parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState flags buffer location

#elif defined (GHC_9_0)

parse :: String -> DynFlags -> String -> ParseResult (Located HsModule)
parse filename flags str = unP GHC.Parser.parseModule parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState flags buffer location

#elif defined (GHC_9_2) || defined (GHC_9_4)

parse :: String -> DynFlags -> String -> ParseResult (Located HsModule)
parse filename flags str = unP GHC.Parser.parseModule parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = initParserState (initParserOpts flags) buffer location

#else
      {- defined (GHC_9_6) || defined (GHC_9_8) || defined (GHC_9_10) -}

parse :: String -> DynFlags -> String -> ParseResult (Located (HsModule GhcPs))
parse filename flags str = unP GHC.Parser.parseModule parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = initParserState (initParserOpts flags) buffer location

#endif

parsePragmasIntoDynFlags :: DynFlags -> FilePath -> String -> IO (Maybe DynFlags)
parsePragmasIntoDynFlags flags filepath str =

#if defined (GHC_8_8) || defined (GHC_8_10) || defined (GHC_9_0)

  catchErrors $ do
    let opts = getOptions flags
                      (stringToStringBuffer str) filepath
    (flags, _, _) <- parseDynamicFilePragma flags opts
    return $ Just flags
  where
    catchErrors :: IO (Maybe DynFlags) -> IO (Maybe DynFlags)
    catchErrors act = handleGhcException reportGhcException
                        (handleSourceError reportSourceErr act)

    reportGhcException e = do
      print e; return Nothing

    reportSourceErr msgs = case sDocs of
        [] -> return Nothing
        sDoc : _ -> do putStrLn sDoc; return Nothing
      where
        sDocs = [ showSDoc flags msg | msg <- pprErrMsgBagWithLoc $ srcErrorMessages msgs ]

#elif defined (GHC_9_2)

  catchErrors $ do
    let opts = getOptions flags
                      (stringToStringBuffer str) filepath
    (flags, _, _) <- parseDynamicFilePragma flags opts
    return $ Just flags
  where
    catchErrors :: IO (Maybe DynFlags) -> IO (Maybe DynFlags)
    catchErrors act = handleGhcException reportGhcException
                        (handleSourceError reportSourceErr act)

    reportGhcException e = do
      print e; return Nothing

    reportSourceErr msgs = case sDocs of
        [] -> return Nothing
        sDoc : _ -> do putStrLn sDoc; return Nothing
      where
        sDocs = [ showSDoc flags msg | msg <- pprMsgEnvelopeBagWithLoc $ srcErrorMessages msgs ]

#elif defined (GHC_9_4)

  catchErrors $ do
    let (_, opts) = getOptions (initParserOpts flags)
                      (stringToStringBuffer str) filepath
    (flags, _, _) <- parseDynamicFilePragma flags opts
    return $ Just flags
  where
    catchErrors :: IO (Maybe DynFlags) -> IO (Maybe DynFlags)
    catchErrors act = handleGhcException reportGhcException
                        (handleSourceError reportSourceErr act)

    reportGhcException e = do print e; return Nothing

    reportSourceErr msgs = case sDocs of
        [] -> return Nothing
        sDoc : _ -> do putStrLn sDoc; return Nothing
      where
        sDocs = [ showSDoc flags msg | msg <- pprMsgEnvelopeBagWithLoc . getMessages $ srcErrorMessages msgs ]

#elif defined (GHC_9_6)

  catchErrors $ do
    let (_, opts) = getOptions (initParserOpts flags)
                      (stringToStringBuffer str) filepath
    (flags, _, _) <- parseDynamicFilePragma flags opts
    return $ Just flags
  where
    catchErrors :: IO (Maybe DynFlags) -> IO (Maybe DynFlags)
    catchErrors act = handleGhcException reportGhcException
                        (handleSourceError reportSourceErr act)

    reportGhcException e = do print e; return Nothing

    reportSourceErr msgs = case sDocs of
      [] -> return Nothing
      sDoc : _ -> do putStrLn sDoc; return Nothing
     where
       sDocs = [ showSDoc flags msg | msg <- pprMsgEnvelopeBagWithLocDefault . getMessages $ srcErrorMessages msgs ]
#else
    {- defined (GHC_9_8) || defined (GHC_9_10) -}

  catchErrors $ do
    let (_, opts) = getOptions (initParserOpts flags)
                      (stringToStringBuffer str) filepath
    (flags, _, _) <- parseDynamicFilePragma flags opts
    return $ Just flags
  where
    catchErrors :: IO (Maybe DynFlags) -> IO (Maybe DynFlags)
    catchErrors act = handleGhcException reportGhcException
                        (handleSourceError reportSourceErr act)

    reportGhcException e = do print e; return Nothing

    reportSourceErr msgs = case sDocs of
        [] -> return Nothing
        sDoc : _ -> do putStrLn sDoc; return Nothing
      where
        sDocs = [ showSDoc flags msg | msg <- pprMsgEnvelopeBagWithLocDefault . getMessages $ srcErrorMessages msgs ]

#endif

idNot :: RdrName
idNot = mkVarUnqual (fsLit "not")

isNegated :: HsExpr GhcPs -> Bool
#if defined (GHC_8_8) || defined (GHC_8_10) || defined (GHC_9_0) || defined (GHC_9_2)

isNegated (HsApp _ (L _ (HsVar _ (L _ id))) _) = id == idNot
isNegated (HsPar _ (L _ e)) = isNegated e
isNegated _ = False

#else
   {- defined (GHC_9_4) || defined (GHC_9_6) || defined (GHC_9_8) || defined (GHC_9_10) -}

isNegated (HsApp _ (L _ (HsVar _ (L _ id))) _) = id == idNot
isNegated (HsPar _ _ (L _ e) _) = isNegated e
isNegated _ = False

#endif

#if defined (GHC_8_8) || defined (GHC_8_10) || defined (GHC_9_0)

analyzeExpr :: DynFlags -> Located (HsExpr GhcPs) -> IO ()
analyzeExpr flags (L loc expr) = do
  case expr of
    HsApp _ (L _ (HsVar _ (L _ id))) (L _ e)
        | id == idNot, isNegated e ->
            putStrLn (showSDoc flags (ppr loc)
                      ++ " : lint : double negation "
                      ++ "`" ++ showSDoc flags (ppr expr) ++ "'")
    _ -> return ()

#else
   {- defined (GHC_9_2) || defined (GHC_9_4) || defined (GHC_9_6) || defined (GHC_9_8) || defined (GHC_9_10) -}

analyzeExpr :: DynFlags -> LocatedA (HsExpr GhcPs) -> IO ()
analyzeExpr flags (L loc expr) = do
  case expr of
    HsApp _ (L _ (HsVar _ (L _ id))) (L _ e)
        | id == idNot, isNegated e ->
            putStrLn (showSDoc flags (ppr (locA loc))
                      ++ " : lint : double negation "
                      ++ "`" ++ showSDoc flags (ppr expr) ++ "'")
    _ -> return ()
#endif

#if defined (GHC_8_8) || defined (GHC_8_10)

analyzeModule :: DynFlags -> Located (HsModule GhcPs) -> ApiAnns -> IO ()
analyzeModule flags (L _ modu) _ = sequence_ [analyzeExpr flags e | e <- universeBi modu]

#elif defined (GHC_9_0)

analyzeModule :: DynFlags -> Located HsModule -> ApiAnns -> IO ()
analyzeModule flags (L _ modu) _ = sequence_ [analyzeExpr flags e | e <- universeBi modu]

#elif defined (GHC_9_2) || defined (GHC_9_4)

analyzeModule :: DynFlags -> Located HsModule -> IO ()
analyzeModule flags (L _ modu) = sequence_ [analyzeExpr flags e | e <- universeBi modu]

#else
   {- defined (GHC_9_6) || defined (GHC_9_8) || defined (GHC_9_10 )-}

analyzeModule :: DynFlags -> Located (HsModule GhcPs) -> IO ()
analyzeModule flags (L _ modu) = sequence_ [analyzeExpr flags e | e <- universeBi modu]

#endif

#if defined (GHC_8_8)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      s <- readFile' file
      flags <- parsePragmasIntoDynFlags (defaultDynFlags fakeSettings fakeLlvmConfig) file s
      whenJust flags $ \flags ->
         case parse file (flags `gopt_set` Opt_KeepRawTokenStream) s of
            PFailed _ loc err -> report flags $ unitBag $ mkPlainErrMsg flags loc err
            POk s m -> do
              let (wrns, errs) = getMessages s flags
              report flags wrns
              report flags errs
              when (null errs) $
                analyzeModule flags m $ harvestAnns s
    _ -> fail "Exactly one file argument required"
  where
    report :: DynFlags -> Bag ErrMsg -> IO ()
    report flags msgs =
      sequence_
        [ putStrLn $ showSDoc flags msg
        | msg <- pprErrMsgBagWithLoc msgs
        ]

    harvestAnns :: PState -> (Map.Map ApiAnnKey [SrcSpan], Map.Map SrcSpan [Located AnnotationComment])
    harvestAnns pst =
      ( Map.fromListWith (++) $ annotations pst
      , Map.fromList ((noSrcSpan, comment_q pst) : annotations_comments pst)
      )

#elif defined (GHC_8_10)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      s <- readFile' file
      flags <- parsePragmasIntoDynFlags (defaultDynFlags fakeSettings fakeLlvmConfig) file s
      whenJust flags $ \flags ->
         case parse file (flags `gopt_set` Opt_KeepRawTokenStream) s of
            PFailed s -> report flags $ snd (getMessages s flags)
            POk s m -> do
              let (wrns, errs) = getMessages s flags
              report flags wrns
              report flags errs
              when (null errs) $
                analyzeModule flags m $ harvestAnns s
    _ -> fail "Exactly one file argument required"
  where
    report :: DynFlags -> Bag.Bag ErrMsg -> IO ()
    report flags msgs =
      sequence_
        [ putStrLn $ showSDoc flags msg
        | msg <- pprErrMsgBagWithLoc msgs
        ]

    harvestAnns :: PState -> (Map.Map ApiAnnKey [SrcSpan], Map.Map SrcSpan [Located AnnotationComment])
    harvestAnns pst =
      ( Map.fromListWith (++) $ annotations pst
      , Map.fromList ((noSrcSpan, comment_q pst) : annotations_comments pst)
      )

#elif defined (GHC_9_0)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      s <- readFile' file
      flags <- parsePragmasIntoDynFlags (defaultDynFlags fakeSettings fakeLlvmConfig) file s
      whenJust flags $ \flags ->
         case parse file (flags `gopt_set` Opt_KeepRawTokenStream) s of
            PFailed s -> report flags $ snd (getMessages s flags)
            POk s m -> do
              let (wrns, errs) = getMessages s flags
              report flags wrns
              report flags errs
              when (null errs) $
                analyzeModule flags m $ harvestAnns s
    _ -> fail "Exactly one file argument required"
  where
    report :: DynFlags -> GHC.Data.Bag.Bag ErrMsg -> IO ()
    report flags msgs =
      sequence_
        [ putStrLn $ showSDoc flags msg
        | msg <- pprErrMsgBagWithLoc msgs
        ]

    harvestAnns :: PState -> ApiAnns
    harvestAnns pst = ApiAnns {
              apiAnnItems = Map.fromListWith (++) $ annotations pst
            , apiAnnEofPos = Nothing
            , apiAnnComments = Map.fromListWith (++) $ annotations_comments pst
            , apiAnnRogueComments = comment_q pst
            }

#elif defined (GHC_9_2)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      s <- readFile' file
      flags <- parsePragmasIntoDynFlags (defaultDynFlags fakeSettings fakeLlvmConfig) file s
      whenJust flags $ \flags ->
         case parse file (flags `gopt_set` Opt_KeepRawTokenStream) s of
            PFailed s -> report flags $ fmap pprError (snd (getMessages s))
            POk s m -> do
              let (wrns, errs) = getMessages s
              report flags $ fmap pprWarning wrns
              report flags $ fmap pprError errs
              when (null errs) $ analyzeModule flags m
    _ -> fail "Exactly one file argument required"
  where
    report :: DynFlags -> GHC.Data.Bag.Bag (MsgEnvelope DecoratedSDoc) -> IO ()
    report flags msgs =
      sequence_
        [ putStrLn $ showSDoc flags msg
        | msg <- pprMsgEnvelopeBagWithLoc msgs
        ]

#elif defined (GHC_9_4)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      s <- readFile' file
      flags <- parsePragmasIntoDynFlags (defaultDynFlags fakeSettings fakeLlvmConfig) file s
      whenJust flags $ \flags ->
         case parse file (flags `gopt_set` Opt_KeepRawTokenStream)s of
            PFailed s -> report flags $ GhcPsMessage <$> snd (getPsMessages s)
            POk s m -> do
              let (wrns, errs) = getPsMessages s
              report flags $ fmap GhcPsMessage wrns
              report flags $ fmap GhcPsMessage errs
              when (null errs) $ analyzeModule flags m
    _ -> fail "Exactly one file argument required"
  where
    report :: DynFlags -> Messages GhcMessage -> IO ()
    report flags msgs = do
      logger <- initLogger
      let opts = initDiagOpts flags
      printMessages logger opts msgs

#else
   {- defined (GHC_9_6) || defined (GHC_9_8) || defined (GHC_910) -}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      s <- readFile' file
      flags <- parsePragmasIntoDynFlags (defaultDynFlags fakeSettings) file s
      whenJust flags $ \flags ->
         case parse file (flags `gopt_set` Opt_KeepRawTokenStream)s of
            PFailed s -> report flags $ GhcPsMessage <$> snd (getPsMessages s)
            POk s m -> do
              let (wrns, errs) = getPsMessages s
              report flags $ fmap GhcPsMessage wrns
              report flags $ fmap GhcPsMessage errs
              when (null errs) $ analyzeModule flags m
    _ -> fail "Exactly one file argument required"
  where
    report :: DynFlags -> Messages GhcMessage -> IO ()
    report flags msgs = do
      let opts = initDiagOpts flags
          print_config = initPrintConfig flags
      logger <- initLogger
      printMessages logger print_config opts msgs

#endif

#if defined (GHC_8_8)

fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])

#elif defined (GHC_8_10) || defined (GHC_9_0) || defined (GHC_9_2) || defined (GHC_9_4)

fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []

#else
   {- defined (GHC_9_6) || defined (GHC_9_8) || defined (GHC_9_10 )-}

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
       }

    toolSettings = ToolSettings {
         toolSettings_opt_P_fingerprint=fingerprint0
       }

    platformMisc=PlatformMisc {
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
      , platformMini=PlatformMini {
            platformMini_arch=ArchUnknown
          , platformMini_os=OSUnknown
          }
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

    platform=genericPlatform

#else
   {- defined (GHC_9_4) || defined (GHC_9_6) || defined (GHC_9_8) || defined (GHC_9_10) -}

    sGhcNameVersion=ghcNameVersion
  , sFileSettings=fileSettings
  , sTargetPlatform=platform
  , sPlatformMisc=platformMisc
  , sToolSettings=toolSettings
  }
  where
    fileSettings = FileSettings {
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

    platform=genericPlatform

#endif
