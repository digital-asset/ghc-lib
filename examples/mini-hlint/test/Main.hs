-- Copyright (c) 2021, Digital Asset (Switzerland) GmbH and/or
-- its affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

{-# OPTIONS_GHC -Werror=unused-imports -Werror=unused-local-binds -Werror=unused-top-binds -Werror=orphans #-}

import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Golden
import System.Info.Extra
import System.FilePath (replaceExtension)
import Data.Proxy
import Data.Maybe
import Data.List.Extra

import TestUtils

main :: IO ()
main = do
  hsFiles <- findByExtension [".hs"] "test"
  defaultMainWithIngredients ings $
    askOption $ \ config@(StackYaml _) ->
      askOption $ \ resolver@(Resolver _) ->
        askOption $ \ flavor@(GhcFlavor _) ->
          goldenTests config resolver flavor hsFiles
  where
    ings =
      includingOptions
        [ Option (Proxy :: Proxy StackYaml)
        , Option (Proxy :: Proxy Resolver)
        , Option (Proxy :: Proxy GhcFlavor)
        ]
      : defaultIngredients

-- Decide if a file is good to test.
runTest :: GhcVersion -> String -> Bool
runTest flavor f =
  (isNothing . stripInfix "Main.hs" $ f) &&
  ((isNothing . stripInfix "MiniHlintTest_respect_dynamic_pragma.hs" $ f) || (flavor >= Ghc8101)) &&
  ((isNothing . stripInfix "MiniHlintTest_non_fatal_error.hs" $ f) || (flavor >= Ghc8101))

goldenTests :: StackYaml -> Resolver -> GhcFlavor -> [FilePath] -> TestTree
goldenTests stackYaml resolver (GhcFlavor ghcFlavor) hsFiles =
  testGroup "mini-hlint tests"
    [ goldenVsString
         testName
         expectFile
         genStringAction
    | hsFile <- filter (runTest ghcFlavor) hsFiles
    , let testName = hsFile
    , let expectFile = replaceExtension hsFile $ (if isWindows then ".windows" else "") ++ ".expect"
    , let genStringAction = stack stackYaml resolver $ "--no-terminal exec -- mini-hlint " ++ hsFile
  ]
