{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

module Main (main) where

import "ghc-lib" GHC
import "ghc-lib" Config
import "ghc-lib" DynFlags
import "ghc-lib" Platform
import "ghc-lib" StringBuffer
import "ghc-lib" Fingerprint
import "ghc-lib" Lexer
import "ghc-lib" RdrName
import "ghc-lib" ErrUtils
import qualified "ghc-lib" Parser
import "ghc-lib" FastString
import "ghc-lib" Outputable
import "ghc-lib" SrcLoc

import System.Environment
import System.IO.Extra


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
    HsApp _ x y -> do
        analyzeExpr flags x
        analyzeExpr flags y
    HsPar _ x -> analyzeExpr flags x
    HsIf _ _ c t f -> do
        analyzeExpr flags c
        analyzeExpr flags t
        analyzeExpr flags f
    OpApp _ x y z -> do
        analyzeExpr flags x
        analyzeExpr flags y
        analyzeExpr flags z
    _ -> return ()

analyzeModule :: DynFlags -> Located (HsModule GhcPs) -> IO ()
analyzeModule flags modu = sequence_
  [ analyzeExpr flags expr
  | L _ HsModule {hsmodDecls=decls} <- [modu]
  , L _ (ValD _ FunBind{fun_matches=MG {mg_alts=(L _ matches)}}) <- decls
  , L _ Match {m_grhss=GRHSs {grhssGRHSs=rhss}} <- matches
  , L _ (GRHS _ _ expr) <- rhss
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      s <- readFile' file
      let flags = defaultDynFlags fakeSettings fakeLlvmConfig
      case parse file flags s of
        POk _ m -> analyzeModule flags m
        PFailed _ ->
          -- putStrLn $ showSDoc flags $ pprLocErrMsg $ mkPlainErrMsg flags loc err
          putStrLn "Fix me" -- to do; call `getErrorMessages` to replace the old code above
    _ -> fail "Exactly one file argument required"
