{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (FilePath)
import Control.Applicative
import Data.Maybe (fromMaybe)
import System.Environment

import Turtle
import Turtle.Options
import Data.Optional
import qualified Data.Text as T
import Control.Monad.Managed
import qualified Control.Foldl as Fold
import Filesystem.Path.CurrentOS (encodeString)

import Automation.Github (deployRelease)
import Automation.CabalVersion (readVersion)
import qualified Automation.CabalVersion as MyVersion

main = do
  version <- readVersion "hack-assembler.cabal"
  token <- T.pack <$> getEnv "EASONCXZ_GITHUB_OAUTH_TOKEN"
  inTravis <- lookupEnv "TRAVIS"
  when (inTravis /= Nothing) $ do
    shell "git config --global user.name \"Travis CI\"" empty
    shell "git config --global user.email \"travis@travis-ci.org\"" empty
    return ()
  let repoUrl = T.concat ["https://", token, "@github.com/easoncxz/homebrew-tap.git"]
  proc "git" ["clone", repoUrl] empty
  with (pushd "homebrew-tap") $ \_ -> do
    let sourceTarUrl = T.concat
          [ "https://github.com/easoncxz/hack-assembler/archive/v"
          , T.pack . MyVersion.asString $ version
          ,".tar.gz"
          ]
        sourceTarPath = T.pack $ "v" ++ MyVersion.asString version ++ ".tar.gz"
    proc "curl" ["-OL", sourceTarUrl] empty
    Just shasumLine <- fmap lineToText <$>
      fold (inproc "shasum" ["-a", "256", sourceTarPath] empty) Fold.head
    let shasum:_ = T.words shasumLine
    let formulaPath = "Formula/hack-assembler.rb"
    oldFormula <- fold (input formulaPath) Fold.list :: IO [Line]
    let newFormula = inproc "../automation/update_formula.rb"
          [ "-s", sourceTarUrl
          , "-c", shasum
          ] (select oldFormula) :: Shell Line
    output formulaPath newFormula
    buildNum <- fromMaybe "local-build" <$> lookupEnv "TRAVIS_BUILD_NUMBER"
    commitId <- fromMaybe "local-commit" <$> lookupEnv "TRAVIS_COMMIT"
    proc "git"
      [ "commit"
      , "-am"
      , T.pack $
        "TravisCI/hack-assembler: Build #"
          ++ buildNum
          ++ " (commit " ++ commitId ++ ")"
      ] empty
    shell "git push" empty
