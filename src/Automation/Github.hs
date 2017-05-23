{-# LANGUAGE OverloadedStrings #-}

module Automation.Github where

import Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import Control.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
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

import Automation.Misc
import qualified Automation.CabalVersion as CV

-- | URL to the tarball Github bundles for us
sdistUrl :: Version -> SdistUrl
sdistUrl version =
  -- Let's use the .tar.gz built by Github --- it has the `stack.yaml`
  SdistUrl . T.pack $
    "https://github.com/easoncxz/hack-assembler/archive/v"
      ++ CV.asString version
      ++ ".tar.gz"

-- | sha256 of that tarball
sdistSha256 :: Version -> IO SdistSha256
sdistSha256 version = do
  let (SdistUrl url) = sdistUrl version
  resp <- Wreq.get (T.unpack url)
  let bytes = resp ^. Wreq.responseBody :: BL.ByteString
  return . SdistSha256 . calcSha256 $ bytes

oauthToken :: IO OAuthToken
oauthToken =
  OAuthToken . T.pack <$> getEnv "EASONCXZ_GITHUB_OAUTH_TOKEN_v2"

