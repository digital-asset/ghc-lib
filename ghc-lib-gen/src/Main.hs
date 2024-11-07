-- Copyright (c) 2019-2022 Digital Asset (Switzerland) GmbH and/or its
-- affiliates. All rights reserved. SPDX-License-Identifier:
-- (Apache-2.0 OR BSD-3-Clause)

module Main (main) where

import Control.Monad
import Ghclibgen
import GhclibgenOpts
import Options.Applicative
import System.Directory

main :: IO ()
main = ghclibgen =<< execParser opts
  where
    opts =
      info
        (helper <*> ghclibgenVersion <*> ghclibgenOpts)
        ( fullDesc
            <> header "ghc-lib-gen - ghc-lib cabal file generator"
            <> progDesc "Generate a ghc-lib target Cabal file"
        )

ghclibgen :: GhclibgenOpts -> IO ()
ghclibgen (GhclibgenOpts root _patches target ghcFlavor skipInit cppOpts _resolver) = do
  withCurrentDirectory root $
    case target of
      GhclibParser -> do
        when withInit $ init ghcFlavor
        mangleCSymbols ghcFlavor
        applyPatchStage ghcFlavor
        applyPatchHaddockHs ghcFlavor
        applyPatchNoMonoLocalBinds ghcFlavor
        applyPatchTemplateHaskellLanguageHaskellTHSyntax ghcFlavor
        applyPatchCompilerGHCUnitTypes ghcFlavor
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
      applyPatchTemplateHaskellCabal ghcFlavor
      applyPatchHadrianCabalProject ghcFlavor
      applyPatchHeapClosures ghcFlavor
      applyPatchRtsIncludePaths ghcFlavor
      applyPatchGhcPrim ghcFlavor
      applyPatchDisableCompileTimeOptimizations ghcFlavor
      -- These lines must come before 'generatePrerequisites':
      applyPatchAclocal ghcFlavor -- Do before ./boot && ./configure
      applyPatchFptoolsAlex ghcFlavor
      applyPatchFpFindCxxStdLib ghcFlavor
      generatePrerequisites ghcFlavor
      -- Renamings come after 'generatePrerequisites':
      applyPatchDerivedConstants ghcFlavor -- Needs DerivedConstants.h
      applyPatchHsVersions ghcFlavor
      applyPatchGHCiMessage ghcFlavor -- Needs ghcversion.h
      -- Before placeholder module generation
      applyPatchGhcInternalEventWindowsHsc ghcFlavor
      setupModuleDepsPlaceholders ghcFlavor
