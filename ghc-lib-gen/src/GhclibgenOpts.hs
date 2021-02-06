-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its
-- affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

{-# LANGUAGE LambdaCase #-}
module GhclibgenOpts(
    GhclibgenTarget(..)
  , GhclibgenOpts(..)
  , GhcFlavor(..)
  , ghclibgenVersion
  , ghclibgenOpts
) where

import Control.Applicative
import Options.Applicative
import Data.Maybe
import Data.Version (showVersion)
import Paths_ghc_lib_gen (version)

-- | A ghc-lib-gen target.
data GhclibgenTarget = GhclibParser | Ghclib

-- | The type of ghc-lib-gen options.
data GhclibgenOpts = GhclibgenOpts {
    ghclibgenOpts_root :: !FilePath -- ^ Path to a GHC git repository.
  , ghclibgenOpts_target :: !GhclibgenTarget -- ^ What target?
  , ghclibgenOpts_ghcFlavor :: !GhcFlavor
 }

-- | A parser of the "--ghc-lib" target.
ghclib :: Parser GhclibgenTarget
ghclib = flag' Ghclib
  ( long "ghc-lib"
  <> help "Generate a ghc-lib.cabal"
  )

-- | A parser of the "--ghc-lib-parser" target.
ghclibParser :: Parser GhclibgenTarget
ghclibParser = flag' GhclibParser
  ( long "ghc-lib-parser"
  <> help "Generate a ghc-lib-parser.cabal"
  )

-- | A parser of "--version".
ghclibgenVersion :: Parser (a -> a)
ghclibgenVersion =
  infoOption
  (showVersion version)
  (long "version" <> help "Show version")

-- | A parser of a ghc-lib-gen target: `target := | "--ghc-lib-parser"
-- | "--ghc-lib" | /* nothing */`.
ghclibgenTarget :: Parser GhclibgenTarget
ghclibgenTarget =
  fromMaybe Ghclib Control.Applicative.<$> optional (ghclibParser <|> ghclib)

-- | A parser of ghc-lib-gen options: `opts := STRING target`.
ghclibgenOpts :: Parser GhclibgenOpts
ghclibgenOpts = GhclibgenOpts
  <$> argument str (metavar "GHC_ROOT")
  <*> ghclibgenTarget
  <*> ghcFlavorOpt

-- | We might want to factor this out so we can share it with CI.hs
-- but for now it doesn’t seem worth it and having CI.hs be
-- self-contained simplifies things.
data GhcFlavor = DaGhc881
               | Ghc881
               | Ghc882
               | Ghc883
               | Ghc884
               | Ghc8101
               | Ghc8102
               | Ghc8103
               | Ghc8104
               | Ghc901
               | GhcMaster
    deriving (Show, Eq, Ord)

ghcFlavorOpt :: Parser GhcFlavor
ghcFlavorOpt = option readFlavor
    ( long "ghc-flavor"
   <> help "The ghc-flavor to test against"
    )

readFlavor :: ReadM GhcFlavor
readFlavor = eitherReader $ \case
    "ghc-9.0.1" -> Right Ghc901
    "ghc-8.10.1" -> Right Ghc8101
    "ghc-8.10.2" -> Right Ghc8102
    "ghc-8.10.3" -> Right Ghc8103
    "ghc-8.10.4" -> Right Ghc8104
    "ghc-8.8.1" -> Right Ghc881
    "ghc-8.8.2" -> Right Ghc882
    "ghc-8.8.3" -> Right Ghc883
    "ghc-8.8.4" -> Right Ghc884
    "da-ghc-8.8.1" -> Right DaGhc881
    "ghc-master" -> Right GhcMaster
    flavor -> Left $ "Failed to parse ghc flavor " <> show flavor <> " expected ghc-master, ghc-9.0.1, ghc-8.8.1, ghc-8.8.2, ghc-8.8.3, ghc-8.8.4, da-ghc-8.8.1, ghc-8.10.1, ghc-8.10.2, ghc-8.10.3 or ghc-8.10.4"
