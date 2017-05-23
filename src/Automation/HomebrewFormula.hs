{-# LANGUAGE OverloadedStrings #-}

module Automation.HomebrewFormula where

import Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Version
import Filesystem.Path ((</>))
import qualified Filesystem.Path as Path
import System.Environment
import Turtle

import Automation.Misc (localScript)
import qualified Automation.CabalVersion as MyVersion

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

-- | Also will be part of the bottle URL
-- See https://github.com/Homebrew/legacy-homebrew/issues/31812
bottleFilename :: Version -> String -> String
bottleFilename version osxVersionName =
  "hack-assembler-" ++ MyVersion.asString version
    ++ "." ++ osxVersionName
    ++ ".bottle.1.tar.gz"
