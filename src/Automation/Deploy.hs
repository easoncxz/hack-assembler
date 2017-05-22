{-# LANGUAGE OverloadedStrings #-}

module Automation.Deploy where

import Data.String.Conversions (cs)
import Data.Version
import Turtle

import Automation.GitCommands
import Automation.Github
import Automation.HomebrewFormula (overwriteSdist, overwriteBottle)
import Automation.Misc

updateFormulaSdist :: Version -> IO ()
updateFormulaSdist version = do
  let sUrl = cs $ sdistUrl version
  sSha256 <- sdistSha256 version
  inTravis <- isInTravis
  when inTravis gitConfig
  rUrl <- tapRepoUrl
  withGitClone rUrl $ do
    overwriteSdist sUrl sSha256
    gitDiff
    gitCommitAM
    gitPush

buildBottle :: Version -> IO ()
buildBottle version = do
  rUrl <- tapRepoUrl
  ExitSuccess <- shell "brew update --verbose" empty
  ExitSuccess <- proc "brew" ["tap", "easoncxz/tap", rUrl] empty
  ExitSuccess <- shell "brew install --verbose --build-bottle easoncxz/tap/hack-assembler" empty
  ExitSuccess <- shell "brew bottle easoncxz/tap/hack-assembler" empty
  return ()
