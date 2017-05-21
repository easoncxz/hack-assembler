{-# LANGUAGE OverloadedStrings #-}

module Automation.HomebrewFormula where

import Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Filesystem.Path ((</>))
import qualified Filesystem.Path as Path
import System.Environment
import Turtle

-- | Wrapper around Ruby CLI
--
-- WARNING: To avoid being CWD-dependent, we're using a hack to
-- figure out where the Ruby script is. Should be OK, since this function
-- is run only as part of a CI process.
updateFormula :: Text -> Text -> [Line] -> IO [Line]
updateFormula sdistUrl sha256 oldFormula = do
  travisDirMS <- lookupEnv "TRAVIS_BUILD_DIR"
  localDirMS <- lookupEnv "HACK_ASSEMBLER_PROJ_DIR"  -- HACK for use on local-dev!
  let Just projectDirS = travisDirMS <|> localDirMS
  let scriptPathT = T.concat
        [ T.pack projectDirS , "/" , "automation/update_formula_sdist.rb" ]
  fold
    (inproc scriptPathT
        [ "--source-tar-url",       sdistUrl
        , "--source-tar-checksum",  sha256
        ]
        (select oldFormula))
    Fold.list
