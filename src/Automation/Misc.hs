
module Automation.Misc where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
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
  bytes
    & (SHA256.hashlazy     :: BL.ByteString -> B.ByteString)  -- hash
    & (BB.byteStringHex    :: B.ByteString  -> BB.Builder)    -- read off bytes
    & (BB.toLazyByteString :: BB.Builder    -> BL.ByteString)
    & (BL.toStrict         :: BL.ByteString -> B.ByteString)
    & (T.decodeUtf8        :: B.ByteString  -> T.Text)        -- technically unsafe

