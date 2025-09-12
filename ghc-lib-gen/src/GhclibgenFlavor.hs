-- Copyright (c) 2019-2025 Digital Asset (Switzerland) GmbH and/or
-- its affiliates. All rights reserved. SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)
{-# LANGUAGE LambdaCase #-}

module GhclibgenFlavor
  ( GhcFlavor (..),
    GhcSeries (..),
    ghcSeries,
  )
where

data GhcFlavor
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
  deriving (Show, Eq, Ord)

data GhcSeries = GHC_8_8 | GHC_8_10 | GHC_9_0 | GHC_9_2 | GHC_9_4 | GHC_9_6 | GHC_9_8 | GHC_9_10 | GHC_9_12 | GHC_9_14 | GHC_9_16
  deriving (Eq, Ord)

instance Show GhcSeries where
  show = \case
    GHC_9_16 -> "ghc-9.16"
    GHC_9_14 -> "ghc-9.14"
    GHC_9_12 -> "ghc-9.12"
    GHC_9_10 -> "ghc-9.10"
    GHC_9_8 -> "ghc-9.8"
    GHC_9_6 -> "ghc-9.6"
    GHC_9_4 -> "ghc-9.4"
    GHC_9_2 -> "ghc-9.2"
    GHC_9_0 -> "ghc-9.0"
    GHC_8_10 -> "ghc-8.10"
    GHC_8_8 -> "ghc-8.8"

ghcSeries :: GhcFlavor -> GhcSeries
ghcSeries = \case
  f | DaGhc881 <= f && f < Ghc8101 -> GHC_8_8
  f | Ghc8101 <= f && f < Ghc901 -> GHC_8_10
  f | Ghc901 <= f && f < Ghc921 -> GHC_9_0
  f | Ghc921 <= f && f < Ghc941 -> GHC_9_2
  f | Ghc941 <= f && f < Ghc961 -> GHC_9_4
  f | Ghc961 <= f && f < Ghc981 -> GHC_9_6
  f | Ghc981 <= f && f < Ghc9101 -> GHC_9_8
  f | Ghc9101 <= f && f < Ghc9121 -> GHC_9_10
  f | Ghc9121 <= f && f < Ghc9141 -> GHC_9_12
  f | Ghc9141 <= f && f < GhcMaster -> GHC_9_14
  GhcMaster -> GHC_9_16
  _ -> error "ghcSeries: impossible case"
