{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Automation.GitCommands where

import Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.String.Conversions (cs)
import System.Directory
import System.Environment (getEnv, lookupEnv)
import Turtle hiding (fromText)
import qualified Turtle

import Automation.Misc (isInTravis, localScript, hackAssemblerCommitId)

gitConfig :: IO ()
gitConfig = do
  ExitSuccess <- shell "git config --global user.name \"Travis CI\"" empty
  ExitSuccess <- shell "git config --global user.email \"travis@travis-ci.org\"" empty
  return ()

gitPush :: IO ()
gitPush = do
  ExitSuccess <- shell "git push" empty
  return ()

gitDiff :: IO ()
gitDiff = do
  ExitSuccess <- shell "git diff --color=always | cat" empty
  return ()

-- | pushd you into a new repo, and cleanup afterwards
withGitClone :: Text -> IO () -> IO ()
withGitClone repoUrl action = do
  let dir = "sub-repo" :: Text
  clean dir
  ExitSuccess <- proc "git" ["clone", repoUrl, dir] empty
  with (pushd $ Turtle.fromText dir) $ \() -> do
    -- verify credentials/authorization
    ExitSuccess <- shell "git push" empty
    ExitSuccess <- shell "git checkout master" empty
    action
  clean dir
  where
    clean dir = do
      dirty <- doesPathExist (cs dir)
      when dirty (removePathForcibly (cs dir))

gitCommitAM :: IO ()
gitCommitAM = do
  buildNum <- fromMaybe "local-build" <$> lookupEnv "TRAVIS_BUILD_NUMBER"
  localCommitId <- hackAssemblerCommitId
  commitIdMaybe <- lookupEnv "TRAVIS_COMMIT"
  let commitId = fromMaybe localCommitId $ commitIdMaybe
  ExitSuccess <- proc "git"
    [ "commit"
    , "-am"
    , T.pack $
      "TravisCI/hack-assembler:"
        ++ " Build #" ++ buildNum
        ++ " (commit " ++ commitId ++ ")"
    ] empty
  return ()
