{-# LANGUAGE OverloadedStrings #-}

module Automation.HomebrewFormula where

import Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
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

-- | Where is the bottle on disk?
-- Also will be part of the bottle URL.
-- See https://github.com/Homebrew/legacy-homebrew/issues/31812
bottlePath :: Version -> OSXVersion -> FilePath
bottlePath version (OSXVersion osx) =
  fromText . T.concat $
    [ "hack-assembler-", T.pack $ MyVersion.asString version
    , ".", osx
    , ".bottle.1.tar.gz"
    ]

-- | Calculate checksum of file
bottleSha256 :: FilePath -> IO BottleSha256
bottleSha256 path =
  case T.unpack <$> toText path of
    Left msg ->
      error . T.unpack $ msg
    Right pathS -> do
      bytes <- BL.readFile pathS
      let sha256 = SHA256.hashlazy bytes
      return . BottleSha256 . T.decodeUtf8 $ sha256

-- | Use `brew bottle` etc. to create a bdist tarball
buildBottle :: Version -> OAuthToken -> IO ()
buildBottle version token = do
  rUrl <- tapRepoUrl token
  ExitSuccess <- shell "brew update --verbose" empty
  ExitSuccess <- proc "brew" ["tap", "easoncxz/tap", rUrl] empty
  ExitSuccess <- shell "brew install --verbose --build-bottle easoncxz/tap/hack-assembler" empty
  ExitSuccess <- shell "brew bottle easoncxz/tap/hack-assembler" empty
  return ()

