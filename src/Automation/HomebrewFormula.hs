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

import Automation.Misc (localScript)

overwriteSdist :: Text -> Text -> IO ()
overwriteSdist sdistUrl sha256 =
  overwriteFormula $ localScript
    "automation/update_formula_sdist.rb"
    [ "--source-tar-url",       sdistUrl
    , "--source-tar-checksum",  sha256
    ]

overwriteBottle :: Text -> Text -> IO ()
overwriteBottle osVersion bottleSha256 =
  overwriteFormula $ localScript
    "automation/update_formula_bottle.rb"
    [ "--os-version", osVersion
    , "--bottle-tar-checksum", bottleSha256
    ]

-- | Overwrite the Formula file
overwriteFormula :: ([Line] -> IO [Line]) -> IO ()
overwriteFormula transform = do
  let formulaPath = "Formula/hack-assembler.rb"
  oldFormula <- fold (input formulaPath) Fold.list
  newFormula <- transform oldFormula
  output formulaPath (select newFormula)
