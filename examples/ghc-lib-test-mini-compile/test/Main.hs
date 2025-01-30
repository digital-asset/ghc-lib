-- Copyright (c) 2018 - 2023, Digital Asset (Switzerland) GmbH and/or
-- its affiliates. All rights reserved. SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

{-# OPTIONS_GHC -Werror=unused-imports -Werror=unused-local-binds -Werror=unused-top-binds -Werror=orphans #-}

import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.HUnit
import Data.Proxy
import Data.Maybe
import Data.List.Extra

import TestUtils
import System.Process.Extra
import System.IO.Extra
import System.Directory

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  putStrLn $ "Current directory: " ++ currentDir
  defaultMainWithIngredients ings $
    askOption $ \ cmd@(CommandFile _) ->
      askOption $ \ config@(StackYaml _) ->
        askOption $ \ resolver@(Resolver _) ->
          askOption $ \ flavor@(GhcFlavor _) -> do
              tests cmd config resolver flavor
  where
    ings =
      includingOptions
        [ Option (Proxy :: Proxy CommandFile)
        , Option (Proxy :: Proxy StackYaml)
        , Option (Proxy :: Proxy Resolver)
        , Option (Proxy :: Proxy GhcFlavor)
       ]
      : defaultIngredients

tests :: CommandFile -> StackYaml -> Resolver -> GhcFlavor -> TestTree
tests miniCompile _stackYaml _stackResolver ghcFlavor =
  testGroup "All tests" [ testCase "MiniCompileTestHs" $ testMiniCompileTestHs miniCompile ghcFlavor]

testMiniCompileTestHs :: CommandFile -> GhcFlavor -> IO ()
testMiniCompileTestHs (CommandFile miniCompile) ghcFlavor = do
  cmd <- readFile' miniCompile
  out <- systemOutput_ $
    cmd ++ case ghcSeries ghcFlavor of
             s | s < GHC_9_14 -> "test/MiniCompileTest.hs"
             s | otherwise -> "test/MiniCompileTestGhcInternalPrim.hs"
  assertBool "MiniCompileTest.hs" (isJust $ stripInfix "$tc'TyCon :: TyCon" out)
