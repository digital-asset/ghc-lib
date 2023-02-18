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

main :: IO ()
main = do
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
tests miniCompile _stackYaml _stackResolver _ghcFlavor = testGroup " All tests"
  [ testCase "MiniCompileTest.hs" $ testMiniCompileTestHs miniCompile ]

testMiniCompileTestHs :: CommandFile -> IO ()
testMiniCompileTestHs (CommandFile miniCompile) = do
  cmd <- readFile' miniCompile
  out <- systemOutput_ $ cmd ++ "test/MiniCompileTest.hs"
  assertBool "MiniCompileTest.hs" (isJust $ stripInfix "$tc'TyCon :: TyCon" out)
