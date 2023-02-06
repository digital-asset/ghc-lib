-- Copyright (c) 2021-2022, Digital Asset (Switzerland) GmbH and/or
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
  ([ testCase "MiniHlint.hs" $ testMiniHlintHs stackYaml stackResolver
   , testCase "MiniHlint_fail_unknown_pragma.hs" $ testMiniHlintFailUnknownPragmaHs stackYaml stackResolver
   , testCase "MiniHlint_fatal_error.hs" $ testMiniHlintFatalErrorHs stackYaml stackResolver ] ++
   [ testCase "MiniHlint_non_fatal_error.hs" $ testMiniHlintNonFatalErrorHs stackYaml stackResolver | ghcFlavor >= Ghc8101 ] ++
   [ testCase "MiniHlint_respect_dynamic_pragma.hs" $ testMiniHlintRespectDynamicPragmaHs stackYaml stackResolver | ghcFlavor >= Ghc8101 ]
  )

testMiniHlintHs :: StackYaml -> Resolver -> IO ()
testMiniHlintHs stackYaml stackResolver = do
  out <- stack stackYaml stackResolver $ "--silent --no-terminal exec -- ghc-lib-test-mini-hlint " ++ "test/MiniHlintTest.hs"
  assertBool "MiniHlint.hs" (isJust $ stripInfix "lint : double negation" (toString out))

testMiniHlintFailUnknownPragmaHs :: StackYaml -> Resolver -> IO ()
testMiniHlintFailUnknownPragmaHs stackYaml stackResolver = do
  out <- stack stackYaml stackResolver $ "--silent --no-terminal exec -- ghc-lib-test-mini-hlint " ++ "test/MiniHlintTest_fail_unknown_pragma.hs"
  assertBool "MiniHlint_fail_unknown_pragma.hs" (isJust $ stripInfix "Unsupported extension" (toString out))

testMiniHlintFatalErrorHs :: StackYaml -> Resolver -> IO ()
testMiniHlintFatalErrorHs stackYaml stackResolver = do
  out <- stack stackYaml stackResolver $ "--silent --no-terminal exec -- ghc-lib-test-mini-hlint " ++ "test/MiniHlintTest_fatal_error.hs"
  assertBool "MiniHlint_fatal_error.hs" (isJust $ stripInfix "parse error" (toString out))

testMiniHlintNonFatalErrorHs :: StackYaml -> Resolver -> IO ()
testMiniHlintNonFatalErrorHs stackYaml stackResolver = do
  out <- stack stackYaml stackResolver $ "--silent --no-terminal exec -- ghc-lib-test-mini-hlint " ++ "test/MiniHlintTest_non_fatal_error.hs"
  assertBool "MiniHlint_non_fatal_error.hs" (isJust $ stripInfix "Found `qualified' in postpositive position" (toString out))

testMiniHlintRespectDynamicPragmaHs :: StackYaml -> Resolver -> IO ()
testMiniHlintRespectDynamicPragmaHs stackYaml stackResolver = do
  out <- stack stackYaml stackResolver $ "--silent --no-terminal exec -- ghc-lib-test-mini-hlint " ++ "test/MiniHlintTest_respect_dynamic_pragma.hs"
  assertEqual "MiniHlint_respect_dynamic_pragma.hs" (toString out) ""
