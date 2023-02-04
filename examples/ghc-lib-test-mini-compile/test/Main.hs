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
import Data.ByteString.Lazy.UTF8

import TestUtils

main :: IO ()
main = do
  defaultMainWithIngredients ings $
    askOption $ \ config@(StackYaml _) ->
      askOption $ \ resolver@(Resolver _) ->
        askOption $ \ flavor@(GhcFlavor _) -> do
          tests config resolver flavor
  where
    ings =
      includingOptions
        [ Option (Proxy :: Proxy StackYaml)
        , Option (Proxy :: Proxy Resolver)
        , Option (Proxy :: Proxy GhcFlavor)
        ]
      : defaultIngredients

tests :: StackYaml -> Resolver -> GhcFlavor -> TestTree
tests stackYaml@(StackYaml yaml) stackResolver@(Resolver resolver) (GhcFlavor ghcFlavor) = testGroup " All tests"
  [ testCase "MiniCompileTest.hs" $ testMiniCompileTestHs stackYaml stackResolver]

testMiniCompileTestHs :: StackYaml -> Resolver -> IO ()
testMiniCompileTestHs stackYaml stackResolver = do
  out <- stack stackYaml stackResolver $ "--no-terminal exec -- ghc-lib-test-mini-compile " ++ "test/MiniCompileTest.hs"
  assertBool "MiniCompileTest.hs" (isJust $ stripInfix "$tc'TyCon :: TyCon" (toString out))
