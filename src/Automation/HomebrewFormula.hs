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

-- | Touch the Formula file
updateFormulaFile :: Text -> Text -> IO ()
updateFormulaFile sdistUrl sdistSha256 = do
  let formulaPath = "Formula/hack-assembler.rb"
  oldFormula <- fold (input formulaPath) Fold.list
  newFormula <- updateFormulaSource sdistUrl sdistSha256 oldFormula
  output formulaPath (select newFormula)

-- | Wrapper around Ruby CLI; actually pure
--
-- WARNING: To avoid being CWD-dependent, we're using a hack to
-- figure out where the Ruby script is. Should be OK, since this function
-- is run only as part of a CI process.
updateFormulaSource :: Text -> Text -> [Line] -> IO [Line]
updateFormulaSource sdistUrl sha256 oldFormula = do
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
