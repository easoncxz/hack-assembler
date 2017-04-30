module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Applicative
import Control.Monad

import Lib (assemble)

main :: IO ()
main = do
  src <- map T.pack . lines <$> getContents
  case assemble src of
    Left error ->
      putStrLn . show $ error
    Right out ->
      mapM_ T.putStrLn out
