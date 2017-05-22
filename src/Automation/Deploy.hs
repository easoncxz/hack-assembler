{-# LANGUAGE OverloadedStrings #-}

module Automation.Deploy where

import Data.String.Conversions (cs)
import Data.Version
import Turtle

import Automation.GitCommands
import Automation.Github
import Automation.HomebrewFormula
import Automation.Misc

updateFormulaSdist :: Version -> IO ()
updateFormulaSdist version = do
  let sUrl = cs $ sdistUrl version
  sSha256 <- sdistSha256 version
  inTravis <- isInTravis
  when inTravis gitConfig
  rUrl <- repoUrl
  withGitClone rUrl $ do
    updateFormulaFile sUrl sSha256
    gitDiff
    gitCommitAM
    gitPush
