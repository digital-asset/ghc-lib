-- Copyright (c) 2021, Digital Asset (Switzerland) GmbH and/or
-- its affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

{-# LANGUAGE LambdaCase #-}

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.Options
import System.Environment
import System.Info.Extra
import System.Directory
import System.Process.Extra
import System.FilePath (takeBaseName, replaceExtension)
import Data.Typeable (Typeable)
import Data.Tagged
import Data.Proxy
import Data.Maybe
import Data.List.Extra
import Data.ByteString.Lazy.UTF8
import Data.Functor

newtype StackYaml = StackYaml (Maybe String)
  deriving (Eq, Ord, Typeable)
-- Give as a path relative to the directory containing
-- mini-hlint.cabal. e.g ../../stack.yaml

newtype Resolver = Resolver (Maybe String)
  deriving (Eq, Ord, Typeable)

data GhcVersion = DaGhc881
                | Ghc881
                | Ghc882
                | Ghc883
                | Ghc884
                | Ghc8101
                | Ghc8102
                | Ghc8103
                | Ghc8104
                | Ghc8105
                | Ghc901
                | Ghc921
                | GhcMaster
  deriving (Eq, Ord, Typeable)

instance Show GhcVersion where
  show = showGhcVersion

showGhcVersion :: GhcVersion -> String
showGhcVersion = \case
    Ghc921 -> "ghc-9.2.1"
    Ghc901 -> "ghc-9.0.1"
    Ghc8101 -> "ghc-8.10.1"
    Ghc8102 -> "ghc-8.10.2"
    Ghc8103 -> "ghc-8.10.3"
    Ghc8104 -> "ghc-8.10.4"
    Ghc8105 -> "ghc-8.10.5"
    Ghc881 -> "ghc-8.8.1"
    Ghc882 -> "ghc-8.8.2"
    Ghc883 -> "ghc-8.8.3"
    Ghc884 -> "ghc-8.8.4"
    DaGhc881 -> "da-ghc-8.8.1"
    GhcMaster -> "ghc-master"

newtype GhcFlavor = GhcFlavor GhcVersion
  deriving(Eq, Ord, Typeable)

readFlavor :: String -> Maybe GhcFlavor
readFlavor f = GhcFlavor <$>
  case f of
    "ghc-9.2.1" -> Just Ghc921
    "ghc-9.0.1" -> Just Ghc901
    "ghc-8.10.1" -> Just Ghc8101
    "ghc-8.10.2" -> Just Ghc8102
    "ghc-8.10.3" -> Just Ghc8103
    "ghc-8.10.4" -> Just Ghc8104
    "ghc-8.10.5" -> Just Ghc8105
    "ghc-8.8.1" -> Just Ghc881
    "ghc-8.8.2" -> Just Ghc882
    "ghc-8.8.3" -> Just Ghc883
    "ghc-8.8.4" -> Just Ghc884
    "da-ghc-8.8.1" -> Just DaGhc881
    "ghc-master" -> Just GhcMaster
    _ -> Nothing

instance IsOption GhcFlavor where
  defaultValue = GhcFlavor GhcMaster
  parseValue = readFlavor
  optionName = return "ghc-flavor"
  optionHelp = return "GHC flavor"

instance IsOption StackYaml where
  defaultValue = StackYaml Nothing
  parseValue = Just . StackYaml . Just
  optionName = return "stack-yaml"
  optionHelp = return "Configuration file"

instance IsOption Resolver where
  defaultValue = Resolver Nothing
  parseValue = Just . Resolver . Just
  optionName = return "resolver"
  optionHelp = return "Resolver override"

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
goldenTests (StackYaml stackYaml) (Resolver resolver) (GhcFlavor ghcFlavor) hsFiles =
  testGroup "mini-hlint tests"
    [ goldenVsString
         testName
         expectFile
         genStringAction
    | hsFile <- filter (runTest ghcFlavor) hsFiles
    , let testName = hsFile
    , let expectFile = replaceExtension hsFile $ (if isWindows then ".windows" else "") ++ ".expect"
    , let genStringAction = stack $ "--no-terminal exec -- mini-hlint " ++ hsFile
  ]
  where
    stack :: String -> IO ByteString
    stack action =
      systemOutput_ ("stack " ++
        concatMap (<> " ")
               [ stackYamlOpt stackYaml
               , stackResolverOpt resolver
               ] ++
        action) <&> fromString

    stackYamlOpt :: Maybe String -> String
    stackYamlOpt = \case
      Just stackYaml -> "--stack-yaml " ++ stackYaml
      Nothing -> ""

    stackResolverOpt :: Maybe String -> String
    stackResolverOpt = \case
      Just resolver -> "--resolver " ++ resolver
      Nothing -> ""
