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
  (isNothing . stripInfix "Main.hs" $ f) &&
  ((isNothing . stripInfix "MiniHlintTest_respect_dynamic_pragma.hs" $ f) || (flavor >= Ghc8101)) &&
  ((isNothing . stripInfix "MiniHlintTest_non_fatal_error.hs" $ f) || (flavor >= Ghc8101))

goldenTests :: StackYaml -> Resolver -> GhcFlavor -> [FilePath] -> TestTree
goldenTests stackYaml@(StackYaml yaml) stackResolver@(Resolver resolver) (GhcFlavor ghcFlavor) hsFiles =
  -- Note: You'll get very confused if you load expect files into your
  -- emacs where you automatically delete whitespace at end of line on
  -- save In this case, disable the 'delete-trailing-whitespace' save
  -- hook and set the local buffer variable 'show-trailing-whitespace'
  -- to 't'. E.g. at this time on master, there's this example: 'Found
  -- `qualified' in postpositive position. '
  case (yaml, resolver) of
    -- Running ghc-9.2.1(ghc-9.2.2) with stack 2.7.3(2.7.5) produces
    --   "Stack has not been tested with GHC versions above 9.0, and using 9.2.1, this may fail"
    --   "Stack has not been tested with Cabal versions above 3.4, but version 3.6.0.0 was found, this may fail"
    -- on stderr which being unexpected, causes the golden tests to fail.
    --
    -- I judge it more work than it's worth to put in expect files
    -- configured for this case since the next stack release will fix
    -- it. So, for now just don't run this test.
    -- 
    -- Update 2022-05-27 This is getting tedious. See
    -- https://github.com/commercialhaskell/stack/issues/5758.
    (Just "../../stack-exact.yaml", Nothing) -> -- implicit resolver 'ghc-9.2.2'
      testGroup "mini-hlint tests" []
    (_, Just ghc) | ghc `elem` [
                        "ghc-9.2.1"
                      , "ghc-9.2.2"
                      , "nightly-2022-05-27" -- ghc-9.2.2
                      , "ghc-9.2.3" -- bindist not set up yet
                      ] ->  -- explicit resolvers
      testGroup "mini-hlint tests" []
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
                       if ghcFlavor `elem` [GhcMaster, Ghc941] then
                         case takeFileName hsFile of
                           "MiniHlintTest_fatal_error.hs" ->
                             takeDirectory hsFile </> "MiniHlintTest_fatal_error-ghc-master.hs"
                           "MiniHlintTest_non_fatal_error.hs" ->
                             takeDirectory hsFile </> "MiniHlintTest_non_fatal_error-ghc-master.hs"
                           _ -> hsFile
                       else
                         hsFile
                 in replaceExtension f $ (if isWindows then ".windows" else "") ++ ".expect"
         , let genStringAction = stack stackYaml stackResolver $ "--no-terminal exec -- mini-hlint " ++ hsFile
         ]
