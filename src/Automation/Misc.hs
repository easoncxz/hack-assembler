
module Automation.Misc where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment (lookupEnv)


-- | We're running on Travis, yes?
isInTravis :: IO Bool
isInTravis = do
  t <- lookupEnv "TRAVIS"
  return (t /= Nothing)

-- | Calculate checksum in memory
calcSha256 :: BL.ByteString -> T.Text
calcSha256 bytes =
  BB.lazyByteStringHex bytes
    & BB.toLazyByteString
    & BL.toStrict
    & T.decodeUtf8  -- unsafe

