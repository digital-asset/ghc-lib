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
tests miniHlint _stackYaml@(StackYaml _yaml) _stackResolver@(Resolver _resolver) (GhcFlavor ghcFlavor) = testGroup " All tests"
  ([ testCase "MiniHlint.hs" $ testMiniHlintHs miniHlint
   , testCase "MiniHlint_fail_unknown_pragma.hs" $ testMiniHlintFailUnknownPragmaHs miniHlint
   , testCase "MiniHlint_fatal_error.hs" $ testMiniHlintFatalErrorHs miniHlint ] ++
   [ testCase "MiniHlint_non_fatal_error.hs" $ testMiniHlintNonFatalErrorHs miniHlint | ghcFlavor >= Ghc8101 ] ++
   [ testCase "MiniHlint_respect_dynamic_pragma.hs" $ testMiniHlintRespectDynamicPragmaHs miniHlint | ghcFlavor >= Ghc8101 ]
  )

testMiniHlintHs :: BinaryPath -> IO ()
testMiniHlintHs (BinaryPath miniHlint) = do
  out <- systemOutput_ (miniHlint ++ " " ++ "test/MiniHlintTest.hs")
  assertBool "MiniHlint.hs" (isJust $ stripInfix "lint : double negation" out)

testMiniHlintFailUnknownPragmaHs :: BinaryPath -> IO ()
testMiniHlintFailUnknownPragmaHs (BinaryPath miniHlint) = do
  out <- systemOutput_ (miniHlint ++ " " ++ "test/MiniHlintTest_fail_unknown_pragma.hs")
  assertBool "MiniHlint_fail_unknown_pragma.hs" (isJust $ stripInfix "Unsupported extension" out)

testMiniHlintFatalErrorHs :: BinaryPath -> IO ()
testMiniHlintFatalErrorHs (BinaryPath miniHlint) = do
  out <- systemOutput_ (miniHlint ++ " " ++ "test/MiniHlintTest_fatal_error.hs")
  assertBool "MiniHlint_fatal_error.hs" (isJust $ stripInfix "parse error" out)

testMiniHlintNonFatalErrorHs :: BinaryPath -> IO ()
testMiniHlintNonFatalErrorHs (BinaryPath miniHlint) = do
  out <- systemOutput_ (miniHlint ++ " " ++ "test/MiniHlintTest_non_fatal_error.hs")
  assertBool "MiniHlint_non_fatal_error.hs" (isJust $ stripInfix "Found `qualified' in postpositive position" out)

testMiniHlintRespectDynamicPragmaHs :: BinaryPath -> IO ()
testMiniHlintRespectDynamicPragmaHs (BinaryPath miniHlint) = do
  out <- systemOutput_ (miniHlint ++ " " ++ "test/MiniHlintTest_respect_dynamic_pragma.hs")
  assertEqual "MiniHlint_respect_dynamic_pragma.hs" out ""  -- True if the the test file parses
