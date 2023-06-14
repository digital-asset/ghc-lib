-- Copyright (c) 2019-2022 Digital Asset (Switzerland) GmbH and/or its
-- affiliates. All rights reserved. SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

module Main(main) where

import Ghclibgen
import GhclibgenOpts

import System.Directory
import Options.Applicative
import Control.Monad

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
ghclibgen (GhclibgenOpts root patches target ghcFlavor skipInit cppOpts resolver) = do
  withCurrentDirectory root $
    case target of
      GhclibParser -> do
        when withInit $ init ghcFlavor
        mangleCSymbols ghcFlavor
        applyPatchStage ghcFlavor
        applyPatchHaddockHs ghcFlavor
        applyPatchNoMonoLocalBinds ghcFlavor
        applyPatchTemplateHaskellLanguageHaskellTHSyntax ghcFlavor
        generateGhcLibParserCabal ghcFlavor cppOpts
      Ghclib -> do
        when withInit $ init ghcFlavor
        applyPatchCmmParseNoImplicitPrelude ghcFlavor
        applyPatchRtsBytecodes ghcFlavor
        applyPatchGHCiInfoTable ghcFlavor
        generateGhcLibCabal ghcFlavor cppOpts
  where
    withInit :: Bool
    withInit = not skipInit

    init :: GhcFlavor -> IO ()
    init ghcFlavor = do
        -- A ghc-lib patch but it removes a file and that needs to
        -- happen before calc parser depends.
        applyPatchSystemSemaphore patches ghcFlavor
        applyPatchTemplateHaskellCabal ghcFlavor
        applyPatchHadrianStackYaml ghcFlavor resolver
        applyPatchHeapClosures ghcFlavor
        applyPatchRtsIncludePaths ghcFlavor
        applyPatchGhcPrim ghcFlavor
        applyPatchDisableCompileTimeOptimizations ghcFlavor
        -- This line must come before 'generatePrerequisites':
        applyPatchAclocal ghcFlavor -- Do before ./boot && ./configure
        -- This invokes 'stack' strictly configured by
        -- 'hadrian/stack.yaml' (which may be influenced by
        -- `applyPatchHadrianStackYaml`).
        generatePrerequisites ghcFlavor
        -- Renamings come after 'generatePrerequisites':
        applyPatchDerivedConstants ghcFlavor -- Needs DerivedConstants.h
        applyPatchHsVersions ghcFlavor
        applyPatchGHCiMessage ghcFlavor -- Needs ghcversion.h
