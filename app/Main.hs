module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Applicative
import Control.Monad
import System.IO
import System.Environment

import Lib (assemble)

main :: IO ()
main = do
  src <- map T.pack . lines <$> hGetContents stdin
  case assemble src of
    Left e ->
      putStrLn . show $ e
    Right out ->
      mapM_ (T.hPutStrLn stdout) out
