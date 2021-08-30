-- Copyright (c) 2021, Digital Asset (Switzerland) GmbH and/or
-- its affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)
{-# OPTIONS_GHC -Werror=unused-imports -Werror=unused-local-binds -Werror=unused-top-binds -Werror=orphans #-}
{-# LANGUAGE LambdaCase #-}

module TestUtils where

import Test.Tasty.Options
import Data.Typeable (Typeable)
import Data.ByteString.Lazy.UTF8
import Data.Functor
import System.Process.Extra

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
                | Ghc8106
                | Ghc8107
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
    Ghc8106 -> "ghc-8.10.6"
    Ghc8107 -> "ghc-8.10.7"
    Ghc881 -> "ghc-8.8.1"
    Ghc882 -> "ghc-8.8.2"
    Ghc883 -> "ghc-8.8.3"
    Ghc884 -> "ghc-8.8.4"
    DaGhc881 -> "da-ghc-8.8.1"
    GhcMaster -> "ghc-master"

newtype GhcFlavor = GhcFlavor GhcVersion
  deriving(Eq, Ord, Typeable)

readFlavor :: String -> Maybe GhcFlavor
readFlavor = (GhcFlavor <$>) . \case
    "ghc-9.2.1" -> Just Ghc921
    "ghc-9.0.1" -> Just Ghc901
    "ghc-8.10.1" -> Just Ghc8101
    "ghc-8.10.2" -> Just Ghc8102
    "ghc-8.10.3" -> Just Ghc8103
    "ghc-8.10.4" -> Just Ghc8104
    "ghc-8.10.5" -> Just Ghc8105
    "ghc-8.10.6" -> Just Ghc8106
    "ghc-8.10.7" -> Just Ghc8107
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

stack :: StackYaml -> Resolver -> String -> IO ByteString
stack (StackYaml stackYaml) (Resolver resolver) action =
  systemOutput_ ("stack " ++
    concatMap (<> " ")
           [ stackYamlOpt stackYaml
           , resolverOpt resolver
           ] ++
    action) <&> fromString

stackYamlOpt :: Maybe String -> String
stackYamlOpt = maybe "" ("--stack-yaml " ++)

resolverOpt :: Maybe String -> String
resolverOpt = maybe "" ("--resolver " ++)
