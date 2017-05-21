{-# LANGUAGE OverloadedStrings #-}

module Automation.HomebrewFormula where

import Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import qualified Data.Text as T
import qualified Filesystem.Path as Path
import Turtle

-- | Wrapper around Ruby CLI
updateFormula :: Text -> Text -> [Line] -> IO [Line]
updateFormula sdistUrl sha256 oldFormula =
  fold
    (inproc "./automation/update_formula_sdist.rb"
        [ "--source-tar-url",       sdistUrl
        , "--source-tar-checksum",  sha256
        ]
        (select oldFormula))
    Fold.list
