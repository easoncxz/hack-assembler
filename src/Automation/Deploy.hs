{-# LANGUAGE OverloadedStrings #-}

module Automation.Deploy where

import Data.String.Conversions (cs)
import Data.Version
import Turtle

import Automation.GitCommands
import Automation.Github
import Automation.HomebrewFormula
import Automation.Misc
import Automation.Bintray

updateFormulaSdist :: Version -> IO ()
updateFormulaSdist version = do
  let sUrl = sdistUrl version
  sSha256 <- sdistSha256 version
  inTravis <- isInTravis
  when inTravis gitConfig
  token <- oauthToken
  rUrl <- tapRepoUrl token
  withGitClone rUrl $ do
    overwriteSdist sUrl sSha256
    gitDiff
    gitCommitAM "updating source tarball"
    gitPush

updateFormulaBottle :: Version -> IO ()
updateFormulaBottle version = do
  token <- oauthToken
  rUrl <- tapRepoUrl token
  path <- buildBottle version token
  uploadBottle version path
  sha256 <- bottleSha256 path
  withGitClone rUrl $ do
    osx@(OSXVersion osxT) <- getOSXVersionName
    overwriteBottle osx sha256
    gitDiff
    gitCommitAM ("updating bottle for osx " <> osxT)
    gitPush
