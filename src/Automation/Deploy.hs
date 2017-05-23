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

uploadBottle :: Version -> IO ()
uploadBottle version = do
  putStrLn "Pretend that the bottle has arrived at Bintray"

updateFormulaBottle :: Version -> IO ()
updateFormulaBottle version = do
  token <- oauthToken
  rUrl <- tapRepoUrl token
  buildBottle version token
  uploadBottle version
  osx@(OSXVersion osxT) <- getOSXVersionName
  let path = bottlePath version osx
  sha256 <- bottleSha256 path
  withGitClone rUrl $ do
    overwriteBottle osx sha256
    gitDiff
    gitCommitAM ("updating bottle for osx " <> osxT)
    gitPush
