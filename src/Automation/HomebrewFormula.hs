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

-- Wrap around a local script
--
-- WARNING: To avoid being CWD-dependent, we're using a hack to
-- figure out where the Ruby scripts are. Should be OK, since this function
-- is run only as part of a CI process.
localScript :: Text -> [Text] -> ([Line] -> IO [Line])
localScript cmd args stdin = do
  travisDirMS <- lookupEnv "TRAVIS_BUILD_DIR"
  localDirMS <- lookupEnv "HACK_ASSEMBLER_PROJ_DIR"  -- HACK for use on local-dev!
  let Just projectDirT = fmap T.pack $ travisDirMS <|> localDirMS
  let scriptPathT = T.concat [ projectDirT , "/" , cmd]
  fold (inproc scriptPathT args (select stdin)) Fold.list
