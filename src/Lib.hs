
module Lib where

import Control.Applicative
import Control.Monad

import Model
import Parser

doStuff :: IO ()
doStuff =
  putStrLn $ "In `main`, we have done something."
