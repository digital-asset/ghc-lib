-- Copyright (c) 2021-2022, Digital Asset (Switzerland) GmbH and/or
-- its affiliates. All rights reserved. SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

{-# OPTIONS_GHC -Werror=unused-imports -Werror=unused-local-binds -Werror=unused-top-binds -Werror=orphans #-}

import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Golden
import System.Info.Extra
import System.FilePath (replaceExtension, takeFileName, takeDirectory)
import System.FilePath.Posix((</>)) -- Generate / on all platforms
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
    -- We need new expect files for master. It's getting tedious to
    -- maintain this test. Disable those affected for now.
    (isNothing . stripInfix "Main.hs" $ f) &&
    ((isNothing . stripInfix "MiniHlintTest_respect_dynamic_pragma.hs" $ f) || (flavor >= Ghc8101) && (flavor < GhcMaster)) &&
    ((isNothing . stripInfix "MiniHlintTest_non_fatal_error.hs" $ f) || (flavor >= Ghc8101) && (flavor < GhcMaster)) &&
    ((isNothing . stripInfix "MiniHlintTest_fatal_error.hs" $ f) || (flavor < GhcMaster)) &&
    ((isNothing . stripInfix "MiniHlintTest_fail_unknown_pragma.hs" $ f) || (flavor < GhcMaster))

goldenTests :: StackYaml -> Resolver -> GhcFlavor -> [FilePath] -> TestTree
goldenTests stackYaml@(StackYaml yaml) stackResolver@(Resolver resolver) (GhcFlavor ghcFlavor) hsFiles =
  -- Note: You'll get very confused if you load expect files into your
  -- emacs where you automatically delete whitespace at end of line on
  -- save. In this case, disable the 'delete-trailing-whitespace' save
  -- hook and set the local buffer variable 'show-trailing-whitespace'
  -- to 't'. E.g. at this time on master, there's this example: 'Found
  -- `qualified' in postpositive position. '
  case (yaml, resolver) of
    (_, _) ->
      testGroup "mini-hlint tests"
         [ goldenVsString
              testName
              expectFile
              genStringAction
         | hsFile <- filter (runTest ghcFlavor) hsFiles
         , let testName = hsFile
         , let expectFile =
                 let f =
                       if ghcFlavor >= Ghc941 then
                         case takeFileName hsFile of
                           "MiniHlintTest_fatal_error.hs" ->
                             takeDirectory hsFile </> "MiniHlintTest_fatal_error-ghc-master.hs"
                           "MiniHlintTest_non_fatal_error.hs" ->
                             takeDirectory hsFile </> "MiniHlintTest_non_fatal_error-ghc-master.hs"
                           _ -> hsFile
                       else
                         hsFile
                 in replaceExtension f $ (if isWindows then ".windows" else "") ++ ".expect"
         , let genStringAction = stack stackYaml stackResolver $ "--silent --no-terminal exec -- ghc-lib-test-mini-hlint " ++ hsFile
         ]
