module Main where

import "ghc-lib" GHC
import "ghc-lib" Config
import "ghc-lib" DynFlags
import "ghc-lib" Platform
import "ghc-lib" HsSyn
import "ghc-lib" HsExtension
import "ghc-lib" StringBuffer
import "ghc-lib" Fingerprint
import "ghc-lib" Lexer
import "ghc-lib" RdrName
import qualified "ghc-lib" Parser

import FastString
import Outputable
import SrcLoc
import System.Environment
import System.IO.Extra
import Control.Monad
import FastString

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
isNegated (HsApp _ ((L _ (HsVar _ (L _ id)))) _) = id == idNot
isNegated (HsPar _ (L _ e)) = isNegated e
isNegated _ = True

analyzeExpr :: Located (HsExpr GhcPs) -> IO ()
analyzeExpr (L loc expr) = do
  case expr of
    HsPar _ x -> analyzeExpr x
    HsIf _ _ c t f -> do { analyzeExpr c; analyzeExpr t ; analyzeExpr f }
    HsApp _ ((L _ (HsVar _ (L _ id)))) (x@(L _ e)) -> do
        if (id == idNot && isNegated e) then
          putStrLn (showSDocUnsafe (ppr loc) ++ ": double negation")
        else
          analyzeExpr x
    OpApp _ x y z -> do {analyzeExpr x; analyzeExpr y; analyzeExpr z }
    _ -> return ()

analyzeGHRs (L _ (GRHS _ _ expr)) = analyzeExpr expr
analyzeGHRs _ = return ()

analyzeMatch :: Located (Match GhcPs (LHsExpr GhcPs)) -> IO ()
analyzeMatch (L _ (Match {m_grhss=(GRHSs {grhssGRHSs=grhss})})) = do
  forM_ grhss analyzeGHRs
analyzeMatch _ = return ()

analyzeDecl :: Located (HsDecl GhcPs) -> IO ()
analyzeDecl (L _ (ValD _ (FunBind {fun_matches=(MG {mg_alts=(L _ matches)})} ))) = do
  forM_ matches analyzeMatch
analyzeDecl _ = return ()

main :: IO ()
main = do
  [filename] <- getArgs
  s <- readFile' filename
  let flags = defaultDynFlags fakeSettings fakeLlvmConfig
  case parse filename flags s of
    POk _ (L _ (HsModule {hsmodDecls=decls})) -> forM_ decls analyzeDecl
    PFailed _ _ _ -> putStrLn "Parse failure"
