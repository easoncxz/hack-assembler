{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Automation.Github where

import Control.Arrow
import Control.Monad
import Control.Lens
import Data.String (IsString)
import Data.String.Conversions (ConvertibleStrings)
import Data.String.Conversions.Monomorphic (fromString)
import Data.Text (Text)
import Data.Version
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import System.Directory
import System.Environment (getEnv)

import qualified Turtle as Tt
import qualified Control.Foldl as Fold
import qualified Data.Aeson as A
import qualified Data.Aeson.Lens as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Either.Combinators (rightToMaybe)
import qualified Data.HashMap.Lazy as HML
import qualified Filesystem.Path.CurrentOS as P
import Network.Wreq
import qualified Network.URI.Template as URI
import Network.URI.Template (UriTemplate)
import qualified Network.URI.Template.Types as URI

import qualified Automation.CabalVersion as CV

-- | Deploy a source tarball onto Github as a "release"
deployRelease :: Version -> IO (Response BL.ByteString)
deployRelease version = do
  resp <- defineRelease version
  let Just uriTemplate = pluckUploadUrlTemplate resp -- or crash
  uploadSdistTarball version uriTemplate


---


-- | First Github API call: defining the release
defineRelease :: Version -> IO (Response BL.ByteString)
defineRelease version = do
  auth <- requestAuthenticator
  let opts = defaults & auth
  postWith opts
    (githubApi "/repos/easoncxz/hack-assembler/releases")
    (A.Object $ HML.fromList
      [ ("tag_name", A.String . T.pack . CV.asTag $ version)
      , ("target_commitish", A.String "dev")  -- just always the dev branch
      ])

-- | Second Github API call: uploading the tarball
uploadSdistTarball :: Version -> UriTemplate -> IO (Response BL.ByteString)
uploadSdistTarball version uriTemplate = do
  makeSdist
  path <- findSdistTarball version
  bytes <- readSdistTarball path
  let uri = renderUploadUrl version uriTemplate
  auth <- requestAuthenticator
  let opts = defaults & auth
  postWith opts uri bytes


---


-- | Call Stack CLI
makeSdist :: IO ()
makeSdist = do
  Tt.ExitSuccess <- Tt.shell "stack sdist" Tt.empty   -- or crash
  return ()

-- | Read the tarball
readSdistTarball :: Tt.FilePath -> IO B.ByteString
readSdistTarball path =
  B.readFile $ P.encodeString $ path

-- | Ask Stack where the tarball is
findSdistTarball :: Version -> IO Tt.FilePath
findSdistTarball version = do
  Just distDir <-   -- match or crash!
    fmap (fmap Tt.lineToText) $ Tt.fold
      (Tt.inshell "stack path --dist-dir" Tt.empty)
      Fold.head
  let pathT = T.concat
        [ distDir
        , "/hack-assembler-"
        , T.pack (CV.asString version)
        , ".tar.gz"
        ]
  x <- doesFileExist $ T.unpack pathT
  if x then
    return $ P.fromText pathT
  else
    fail "bad version for sdist"

-- | Decide where to upload to
renderUploadUrl :: Version -> UriTemplate -> String
renderUploadUrl version uriTemplate =
  let filename = "hack-assembler-" ++ CV.asString version ++ ".tar.gz"
  in URI.render
      uriTemplate
      [("name", URI.WrappedValue . URI.Single $ filename)]

-- | Pluck out a URL template
pluckUploadUrlTemplate :: Response BL.ByteString -> Maybe UriTemplate
pluckUploadUrlTemplate response =
  let uriTemplateStrM =
        response ^? responseBody . A.key "upload_url" . A._String
          :: Maybe Text
  in rightToMaybe . URI.parseTemplate . T.unpack =<< uriTemplateStrM


---


githubApi :: String -> String
githubApi =
  ("https://api.github.com" ++)

oauthToken :: (ConvertibleStrings String a) => IO a
oauthToken =
  fromString <$> getEnv "EASONCXZ_GITHUB_OAUTH_TOKEN_v2"

requestAuthenticator :: IO (Options -> Options)
requestAuthenticator = do
  token <- oauthToken
  return $ header "Authorization" .~ ["token " `B.append` token]

