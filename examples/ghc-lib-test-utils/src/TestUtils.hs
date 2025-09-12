-- Copyright (c) 2021-2025 Digital Asset (Switzerland) GmbH and/or its
-- affiliates. All rights reserved. SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Werror=unused-imports -Werror=unused-local-binds -Werror=unused-top-binds -Werror=orphans #-}

module TestUtils where

import Data.Typeable (Typeable)
import Test.Tasty.Options

newtype StackYaml = StackYaml (Maybe String)
  deriving (Eq, Ord, Typeable)

-- Give as a path relative to the directory containing
-- ghc-lib-test-mini-hlint.cabal. e.g ../../stack.yaml

newtype Resolver = Resolver (Maybe String)
  deriving (Eq, Ord, Typeable)

data GhcVersion
  = DaGhc881
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
  | Ghc902
  | Ghc921
  | Ghc922
  | Ghc923
  | Ghc924
  | Ghc925
  | Ghc926
  | Ghc927
  | Ghc928
  | Ghc941
  | Ghc942
  | Ghc943
  | Ghc944
  | Ghc945
  | Ghc946
  | Ghc947
  | Ghc948
  | Ghc961
  | Ghc962
  | Ghc963
  | Ghc964
  | Ghc965
  | Ghc967
  | Ghc966
  | Ghc981
  | Ghc982
  | Ghc983
  | Ghc984
  | Ghc985
  | Ghc9101
  | Ghc9102
  | Ghc9103
  | Ghc9121
  | Ghc9122
  | Ghc9141
  | GhcMaster
  deriving (Eq, Ord, Typeable)

data GhcSeries = GHC_8_8 | GHC_8_10 | GHC_9_0 | GHC_9_2 | GHC_9_4 | GHC_9_6 | GHC_9_8 | GHC_9_10 | GHC_9_12 | GHC_9_14 | GHC_9_16
  deriving (Eq, Ord)

instance Show GhcVersion where
  show = showGhcVersion

showGhcVersion :: GhcVersion -> String
showGhcVersion = \case
  Ghc9141 -> "ghc-9.14.1"
  Ghc9122 -> "ghc-9.12.2"
  Ghc9121 -> "ghc-9.12.1"
  Ghc9103 -> "ghc-9.10.3"
  Ghc9102 -> "ghc-9.10.2"
  Ghc9101 -> "ghc-9.10.1"
  Ghc985 -> "ghc-9.8.5"
  Ghc984 -> "ghc-9.8.4"
  Ghc983 -> "ghc-9.8.3"
  Ghc982 -> "ghc-9.8.2"
  Ghc981 -> "ghc-9.8.1"
  Ghc961 -> "ghc-9.6.1"
  Ghc948 -> "ghc-9.4.8"
  Ghc947 -> "ghc-9.4.7"
  Ghc946 -> "ghc-9.4.6"
  Ghc945 -> "ghc-9.4.5"
  Ghc944 -> "ghc-9.4.4"
  Ghc943 -> "ghc-9.4.3"
  Ghc942 -> "ghc-9.4.2"
  Ghc941 -> "ghc-9.4.1"
  Ghc923 -> "ghc-9.2.3"
  Ghc922 -> "ghc-9.2.2"
  Ghc921 -> "ghc-9.2.1"
  Ghc901 -> "ghc-9.0.1"
  Ghc902 -> "ghc-9.0.2"
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
  deriving (Eq, Ord, Typeable)

ghcSeries :: GhcFlavor -> GhcSeries
ghcSeries (GhcFlavor f)
  | DaGhc881 <= f && f < Ghc8101 = GHC_8_8
  | Ghc8101 <= f && f < Ghc901 = GHC_8_10
  | Ghc901 <= f && f < Ghc921 = GHC_9_0
  | Ghc921 <= f && f < Ghc941 = GHC_9_2
  | Ghc941 <= f && f < Ghc961 = GHC_9_4
  | Ghc961 <= f && f < Ghc981 = GHC_9_6
  | Ghc981 <= f && f < Ghc9101 = GHC_9_8
  | Ghc9101 <= f && f < Ghc9121 = GHC_9_10
  | Ghc9121 <= f && f < Ghc9141 = GHC_9_12
  | Ghc9141 <= f && f < GhcMaster = GHC_9_14
  | otherwise = GHC_9_16

readFlavor :: String -> Maybe GhcFlavor
readFlavor =
  (GhcFlavor <$>) . \case
    -- HEAD
    "ghc-master" -> Just GhcMaster
    -- ghc-9.14
    "ghc-9.14.1" -> Just Ghc9141
    -- ghc-9.12
    "ghc-9.12.2" -> Just Ghc9122
    "ghc-9.12.1" -> Just Ghc9121
    -- ghc-9.10
    "ghc-9.10.3" -> Just Ghc9103
    "ghc-9.10.2" -> Just Ghc9102
    "ghc-9.10.1" -> Just Ghc9101
    -- ghc-9.8
    "ghc-9.8.5" -> Just Ghc985
    "ghc-9.8.4" -> Just Ghc984
    "ghc-9.8.3" -> Just Ghc983
    "ghc-9.8.2" -> Just Ghc982
    "ghc-9.8.1" -> Just Ghc981
    -- ghc-9.6
    "ghc-9.6.7" -> Just Ghc967
    "ghc-9.6.6" -> Just Ghc966
    "ghc-9.6.5" -> Just Ghc965
    "ghc-9.6.4" -> Just Ghc964
    "ghc-9.6.3" -> Just Ghc963
    "ghc-9.6.2" -> Just Ghc962
    "ghc-9.6.1" -> Just Ghc961
    -- ghc-9.4
    "ghc-9.4.8" -> Just Ghc948
    "ghc-9.4.7" -> Just Ghc947
    "ghc-9.4.6" -> Just Ghc946
    "ghc-9.4.5" -> Just Ghc945
    "ghc-9.4.4" -> Just Ghc944
    "ghc-9.4.3" -> Just Ghc943
    "ghc-9.4.2" -> Just Ghc942
    "ghc-9.4.1" -> Just Ghc941
    -- ghc-9.2
    "ghc-9.2.8" -> Just Ghc928
    "ghc-9.2.7" -> Just Ghc927
    "ghc-9.2.6" -> Just Ghc926
    "ghc-9.2.5" -> Just Ghc925
    "ghc-9.2.4" -> Just Ghc924
    "ghc-9.2.3" -> Just Ghc923
    "ghc-9.2.2" -> Just Ghc922
    "ghc-9.2.1" -> Just Ghc921
    -- ghc-9.0
    "ghc-9.0.2" -> Just Ghc902
    "ghc-9.0.1" -> Just Ghc901
    -- ghc-8.10
    "ghc-8.10.7" -> Just Ghc8107
    "ghc-8.10.6" -> Just Ghc8106
    "ghc-8.10.5" -> Just Ghc8105
    "ghc-8.10.4" -> Just Ghc8104
    "ghc-8.10.3" -> Just Ghc8103
    "ghc-8.10.2" -> Just Ghc8102
    "ghc-8.10.1" -> Just Ghc8101
    -- ghc-8.8
    "da-ghc-8.8.1" -> Just DaGhc881
    "ghc-8.8.4" -> Just Ghc884
    "ghc-8.8.3" -> Just Ghc883
    "ghc-8.8.2" -> Just Ghc882
    "ghc-8.8.1" -> Just Ghc881
    -- unknown
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

newtype CommandFile = CommandFile String
  deriving (Eq, Ord, Typeable)

instance IsOption CommandFile where
  defaultValue = CommandFile "MISSING"
  parseValue = Just . CommandFile
  optionName = return "test-command"
  optionHelp = return "File containing a command"
