{-# LANGUAGE OverloadedStrings #-}

module Automation.Bintray where

import Data.Version
import Network.Wreq

import Automation.Misc

uploadBottle :: Version -> BottlePath -> IO ()
uploadBottle version (BottlePath pathT) = do
  putStrLn "Pretend that the bottle has arrived at Bintray"

