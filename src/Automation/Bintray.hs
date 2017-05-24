{-# LANGUAGE OverloadedStrings #-}

module Automation.Bintray where

import Control.Lens
import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL.C8
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.HashMap.Lazy as HML
import qualified Data.Text as T
import Data.Version
import System.Environment
import Network.Wreq
import Turtle

import Automation.Misc
import Automation.CabalVersion as MyVersion

getBintrayApiKey :: IO B.ByteString
getBintrayApiKey =
  UTF8.fromString <$> getEnv "EASONCXZ_BINTRAY_API_KEY"

bintrayApi :: String -> String
bintrayApi slash =
  "https://bintray.com/api/v1" <> slash

showResp :: Response (BL.ByteString) -> IO ()
showResp r = do
  putStrLn . show $ r ^. responseStatus
  mapM_ (putStrLn . show) $ r ^. responseHeaders
  BL.C8.putStrLn $ r ^. responseBody
  putStrLn ""

postBottleVersion :: Version -> IO ()
postBottleVersion version = do
  apiKey <- getBintrayApiKey
  r <- postWith
    (defaults & auth ?~ basicAuth "easoncxz" apiKey)
    (bintrayApi "/packages/easoncxz/homebrew-bottles/hack-assembler/versions")
    (A.toJSON . HML.fromList $
      [ (("name" :: String), MyVersion.asString version) ])
  showResp r

putBottleFile :: Version -> BottlePath -> IO ()
putBottleFile version (BottlePath pathT) = do
  let Right baseS = fmap T.unpack . toText . filename . fromText $ pathT  -- may crash
  bytes <- BL.readFile (T.unpack pathT)
  apiKey <- getBintrayApiKey
  r <- putWith
    (defaults
      & auth ?~ basicAuth "easoncxz" apiKey
      & param "publish" .~ ["1"])
    (bintrayApi
      ("/content/easoncxz/homebrew-bottles/hack-assembler"
       <> "/" <> MyVersion.asString version
       <> "/" <> baseS))
    bytes
  showResp r

uploadBottle :: Version -> BottlePath -> IO ()
uploadBottle version path = do
    postBottleVersion version
    putBottleFile version path
