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
import "ghc-lib" ErrUtils
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
isNegated _ = False

analyzeExpr :: DynFlags -> Located (HsExpr GhcPs) -> IO ()
analyzeExpr flags (L loc expr) = do
  case expr of
    HsPar _ x -> analyzeExpr flags x
    HsIf _ _ c t f -> do {
        analyzeExpr flags c
      ; analyzeExpr flags t
      ; analyzeExpr flags f }
    HsApp _ ((L _ (HsVar _ (L _ id)))) (x@(L _ e)) -> do
        if (id == idNot && isNegated e) then
          putStrLn (showSDoc flags (ppr loc)
                    ++ " : lint : double negation "
                    ++ "`" ++ showSDoc flags (ppr expr) ++ "'")
        else
          analyzeExpr flags x
    OpApp _ x y z -> do {
          analyzeExpr flags x
        ; analyzeExpr flags y
        ; analyzeExpr flags z}
    _ -> return ()

analyzeGHRs :: DynFlags -> Located (GRHS GhcPs (LHsExpr GhcPs)) -> IO ()
analyzeGHRs flags (L _ (GRHS _ _ expr)) = analyzeExpr flags expr
analyzeGHRs _ _ = return ()

analyzeMatch :: DynFlags -> Located (Match GhcPs (LHsExpr GhcPs)) -> IO ()
analyzeMatch flags (L _ (Match {m_grhss=(GRHSs {grhssGRHSs=grhss})})) = do
  forM_ grhss (analyzeGHRs flags)
analyzeMatch _ _ = return ()

analyzeDecl :: DynFlags -> Located (HsDecl GhcPs) -> IO ()
analyzeDecl flags (L _ (ValD _ (FunBind {fun_matches=(MG {mg_alts=(L _ matches)})} ))) = do
  forM_ matches (analyzeMatch flags)
analyzeDecl _ _ = return ()

main :: IO ()
main = do
  [filename] <- getArgs
  s <- readFile' filename
  let flags = defaultDynFlags fakeSettings fakeLlvmConfig
  case parse filename flags s of
    POk _ (L _ (HsModule {hsmodDecls=decls})) ->
      forM_ decls (analyzeDecl flags)
    PFailed _ loc err ->
      putStrLn (showSDoc flags (pprLocErrMsg (mkPlainErrMsg flags loc err)))
