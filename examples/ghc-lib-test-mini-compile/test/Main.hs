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
import System.Environment

main :: IO ()
main = do
  unsetEnv "GHC_ENVIRONMENT"
  defaultMainWithIngredients ings $
    askOption $ \ binary@(BinaryPath _) ->
      askOption $ \ config@(StackYaml _) ->
        askOption $ \ resolver@(Resolver _) ->
          askOption $ \ flavor@(GhcFlavor _) -> do
            tests binary config resolver flavor
  where
    ings =
      includingOptions
        [ Option (Proxy :: Proxy BinaryPath)
        , Option (Proxy :: Proxy StackYaml)
        , Option (Proxy :: Proxy Resolver)
        , Option (Proxy :: Proxy GhcFlavor)
        ]
      : defaultIngredients

tests :: BinaryPath -> StackYaml -> Resolver -> GhcFlavor -> TestTree
tests miniCompile _stackYaml@(StackYaml _yaml) _stackResolver@(Resolver _resolver) (GhcFlavor _ghcFlavor) = testGroup " All tests"
  [ testCase "MiniCompileTest.hs" $ testMiniCompileTestHs miniCompile ]

testMiniCompileTestHs :: BinaryPath -> IO ()
testMiniCompileTestHs (BinaryPath miniCompile) = do
  out <- systemOutput_ (miniCompile ++ " " ++ "test/MiniCompileTest.hs")
  assertBool "MiniCompileTest.hs" (isJust $ stripInfix "$tc'TyCon :: TyCon" out)
