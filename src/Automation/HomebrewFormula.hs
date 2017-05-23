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

import Automation.Misc
import qualified Automation.CabalVersion as MyVersion

overwriteSdist :: SdistUrl -> SdistSha256 -> IO ()
overwriteSdist (SdistUrl sdistUrl) (SdistSha256 sha256) =
  overwriteFormula $ localScript
    "automation/update_formula_sdist.rb"
    [ "--source-tar-url", sdistUrl
    , "--source-tar-checksum", sha256
    ]

overwriteBottle :: OSXVersion -> BottleSha256 -> IO ()
overwriteBottle (OSXVersion osx) (BottleSha256 sha256) =
  overwriteFormula $ localScript
    "automation/update_formula_bottle.rb"
    [ "--os-version", osx
    , "--bottle-tar-checksum", sha256
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
bottleFilename :: Version -> OSXVersion -> Text
bottleFilename version (OSXVersion osx) =
  T.concat
    [ "hack-assembler-", T.pack $ MyVersion.asString version
    , ".", osx
    , ".bottle.1.tar.gz"
    ]
