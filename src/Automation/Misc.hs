{-# LANGUAGE OverloadedStrings #-}

module Automation.Misc where

import Control.Applicative
import qualified Control.Foldl as Fold
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment (lookupEnv)
import qualified Turtle

-- | We're running on Travis, yes?
isInTravis :: IO Bool
isInTravis = do
  t <- lookupEnv "TRAVIS"
  return (t /= Nothing)

-- | Calculate checksum in memory
calcSha256 :: BL.ByteString -> T.Text
calcSha256 bytes =
  bytes
    & (SHA256.hashlazy     :: BL.ByteString -> B.ByteString)  -- hash
    & (BB.byteStringHex    :: B.ByteString  -> BB.Builder)    -- read off bytes
    & (BB.toLazyByteString :: BB.Builder    -> BL.ByteString)
    & (BL.toStrict         :: BL.ByteString -> B.ByteString)
    & (T.decodeUtf8        :: B.ByteString  -> T.Text)        -- technically unsafe

-- Wrap around a local script
--
-- WARNING: To avoid being CWD-dependent, we're using a hack to
-- figure out where the Ruby scripts are. Should be OK, since this function
-- is run only as part of a CI process.
localScript
  :: T.Text
  -> [T.Text]
  -> [Turtle.Line]
  -> IO [Turtle.Line]
localScript cmd args stdin = do
  travisDirMS <- lookupEnv "TRAVIS_BUILD_DIR"
  localDirMS <- lookupEnv "HACK_ASSEMBLER_PROJ_DIR"  -- HACK for use on local-dev!
  let Just projectDirT = fmap T.pack $ travisDirMS <|> localDirMS
  let scriptPathT = T.concat [ projectDirT , "/" , cmd]
  Turtle.fold (Turtle.inproc scriptPathT args (Turtle.select stdin)) Fold.list
