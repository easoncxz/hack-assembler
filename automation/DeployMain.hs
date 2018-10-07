{-# LANGUAGE OverloadedStrings #-}

module Main where

import Automation.CabalVersion (readVersion)
import qualified Automation.CabalVersion as CabalVersion
import qualified Data.Text.IO as T
import Turtle

parser :: Parser Text
parser = argText "COMMAND" "version-as-tag | version-as-string"

main = do
  command <- options "Retrieve version from Cabal file" parser
  pkgVersion <- readVersion "hack-assembler.cabal"
  case command of
    "version-as-tag" -> putStrLn (CabalVersion.asTag pkgVersion)
    "version-as-string" -> putStrLn (CabalVersion.asString pkgVersion)
    o -> do
      T.putStrLn $ "Unrecognized command: " <> o
      exit (ExitFailure 1)
