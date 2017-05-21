{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Automation.Github where

import Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import Control.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.String.Conversions (ConvertibleStrings)
import qualified Data.String.Conversions.Monomorphic as CS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Version
import System.Environment (getEnv)
import Turtle
import qualified Network.Wreq as Wreq

import Automation.Misc (calcSha256)
import qualified Automation.CabalVersion as CV

-- | URL to the tarball Github bundles for us
sdistUrl :: Version -> String
sdistUrl version =
  -- Let's use the .tar.gz built by Github --- it has the `stack.yaml`
  "https://github.com/easoncxz/hack-assembler/archive/v"
    ++ CV.asString version
    ++ ".tar.gz"

-- | sha256 of that tarball
sdistSha256 :: Version -> IO Text
sdistSha256 version = do
  let url = sdistUrl version
  resp <- Wreq.get url
  let bytes = resp ^. Wreq.responseBody :: BL.ByteString
  return (calcSha256 bytes)

oauthToken :: (ConvertibleStrings String a) => IO a
oauthToken =
  CS.fromString <$> getEnv "EASONCXZ_GITHUB_OAUTH_TOKEN_v2"

