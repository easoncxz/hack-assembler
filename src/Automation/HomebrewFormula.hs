{-# LANGUAGE OverloadedStrings #-}

module Automation.HomebrewFormula where

import Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import qualified Data.Text as T
import qualified Filesystem.Path as Path
import Turtle

-- | Touch a Formula file in the homebrew-tap repo
updateFormula :: Text -> Text -> IO ()
updateFormula sdistUrl sha256 = do
  -- Assumes the CWD is in the homebrew-tap repo.
  let formulaPath = "Formula/hack-assembler.rb" :: FilePath
  -- Buffer the data into memory, since we need to clobber the file
  oldFormula <- fold (input formulaPath) Fold.list :: IO [Line]
  -- Let failures be before we clobber the file
  newFormula <- fold
    (inproc "../automation/update_formula_sdist.rb"
        [ "--source-tar-url", sdistUrl
        , "--source-tar-checksum", sha256
        ]
        (select oldFormula) :: Shell Line)
    Fold.list :: IO [Line]
  output formulaPath (select newFormula)
