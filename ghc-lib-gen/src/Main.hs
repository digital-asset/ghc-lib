-- Copyright (c) 2019, Digital Asset (Switzerland) GmbH and/or its
-- affiliates. All rights reserved.  SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

module Main(main) where

import System.Directory
import Ghclibgen
import GhclibgenOpts
import Options.Applicative

main :: IO ()
main = ghclibgen =<< execParser opts
  where
    opts =
      info
      (helper <*> ghclibgenVersion <*> ghclibgenOpts)
      (fullDesc
        <> header "ghc-lib-gen - ghc-lib cabal file generator"
        <> progDesc "Generate a ghc-lib target Cabal file"
      )

ghclibgen :: GhclibgenOpts -> IO ()
ghclibgen (GhclibgenOpts root target ghcFlavor) =
  withCurrentDirectory root $
    case target of
      GhclibParser -> do
        init ghcFlavor
        mangleCSymbols ghcFlavor
        applyPatchStage ghcFlavor
        generateGhcLibParserCabal ghcFlavor
      Ghclib -> do
        init ghcFlavor
        generateGhcLibCabal ghcFlavor
  where
    init :: GhcFlavor -> IO ()
    init ghcFlavor = do
        applyPatchHsVersions ghcFlavor
        applyPatchHeapClosures ghcFlavor
        applyPatchRtsIncludePaths ghcFlavor
        applyPatchGhcPrim ghcFlavor
        applyPatchDisableCompileTimeOptimizations ghcFlavor
        -- This invokes 'stack' strictly configured by
        -- 'hadrian/stack.yaml'.
        generatePrerequisites ghcFlavor
