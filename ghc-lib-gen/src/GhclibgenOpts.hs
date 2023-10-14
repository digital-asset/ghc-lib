-- Copyright (c) 2019 - 2023 Digital Asset (Switzerland) GmbH and/or
-- its affiliates. All rights reserved. SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

{-# LANGUAGE LambdaCase #-}
module GhclibgenOpts(
    GhclibgenTarget(..)
  , GhclibgenOpts(..)
  , module GhclibgenFlavor
  , ghclibgenVersion
  , ghclibgenOpts
) where

import Control.Applicative
import Options.Applicative
import Data.Maybe
import Data.Version (showVersion)
import Paths_ghc_lib_gen (version)

import GhclibgenFlavor

-- | A ghc-lib-gen target.
data GhclibgenTarget = GhclibParser | Ghclib

-- | The type of ghc-lib-gen options.
data GhclibgenOpts = GhclibgenOpts {
    ghclibgenOpts_root :: !FilePath -- ^ Path to a GHC git repository.
  , ghclibgenOpts_patches :: !FilePath -- ^ Path to a directory of patches.
  , ghclibgenOpts_target :: !GhclibgenTarget
  , ghclibgenOpts_ghcFlavor :: !GhcFlavor
  , ghclibgenOpts_skipInit :: !Bool
  , ghclibgenOpts_customCppFlags :: ![String]
  , ghclibgenOpts_stackResolver :: !(Maybe String)
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
  <*> argument str (metavar "PATCHES_DIR")
  <*> ghclibgenTarget
  <*> ghcFlavorOpt
  <*> switch
        ( long "skip-init"
        <> help "If enabled, skip initialization steps"
        )
  <*> cppCustomFlagsOpt
  <*> stackResolverOpt

ghcFlavorOpt :: Parser GhcFlavor
ghcFlavorOpt = option readFlavor
    ( long "ghc-flavor"
   <> help "The ghc-flavor to test against"
    )

readFlavor :: ReadM GhcFlavor
readFlavor = eitherReader $ \case
    -- HEAD
    "ghc-master" -> Right GhcMaster

    -- ghc-9.8
    "ghc-9.8.1" -> Right Ghc981

    -- ghc-9.6
    "ghc-9.6.3" -> Right Ghc963
    "ghc-9.6.2" -> Right Ghc962
    "ghc-9.6.1" -> Right Ghc961

    -- ghc-9.4
    "ghc-9.4.7" -> Right Ghc947
    "ghc-9.4.6" -> Right Ghc946
    "ghc-9.4.5" -> Right Ghc945
    "ghc-9.4.4" -> Right Ghc944
    "ghc-9.4.3" -> Right Ghc943
    "ghc-9.4.2" -> Right Ghc942
    "ghc-9.4.1" -> Right Ghc941

    -- ghc-9.2
    "ghc-9.2.8" -> Right Ghc928
    "ghc-9.2.7" -> Right Ghc927
    "ghc-9.2.6" -> Right Ghc926
    "ghc-9.2.5" -> Right Ghc925
    "ghc-9.2.4" -> Right Ghc924
    "ghc-9.2.3" -> Right Ghc923
    "ghc-9.2.2" -> Right Ghc922
    "ghc-9.2.1" -> Right Ghc921

    -- ghc-9.0
    "ghc-9.0.2" -> Right Ghc902
    "ghc-9.0.1" -> Right Ghc901

    -- ghc-8.10
    "ghc-8.10.7" -> Right Ghc8107
    "ghc-8.10.6" -> Right Ghc8106
    "ghc-8.10.5" -> Right Ghc8105
    "ghc-8.10.4" -> Right Ghc8104
    "ghc-8.10.3" -> Right Ghc8103
    "ghc-8.10.2" -> Right Ghc8102
    "ghc-8.10.1" -> Right Ghc8101

    -- ghc-8.8
    "ghc-8.8.4" -> Right Ghc884
    "ghc-8.8.3" -> Right Ghc883
    "ghc-8.8.2" -> Right Ghc882
    "ghc-8.8.1" -> Right Ghc881
    "da-ghc-8.8.1" -> Right DaGhc881

    flavor -> Left $ "Unknown or unsupported flavor \"" ++ flavor ++ "\""

cppCustomFlagsOpt :: Parser [String]
cppCustomFlagsOpt =
  many $
    strOption
      ( long "cpp"
     <> help "CPP options to include in the generated cabal file"
      )

stackResolverOpt :: Parser (Maybe String)
stackResolverOpt =
  optional $
    strOption
      ( long "resolver"
     <> help "the prevailing stack resolver"
      )
