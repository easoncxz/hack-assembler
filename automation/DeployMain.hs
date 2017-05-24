{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as T
import Turtle
import Automation.CabalVersion (readVersion)
import Automation.Deploy

parser :: Parser Text
parser =
  argText "COMMAND" "update-sdist | update-bottle"

main = do
  command <- options "Run a deployment step" parser
  pkgVersion <- readVersion "hack-assembler.cabal"
  case command of
    "update-sdist" ->
      updateFormulaSdist pkgVersion
    "update-bottle" ->
      updateFormulaBottle pkgVersion
    o -> do
      T.putStrLn $ "Unrecognized command: " <> o
      exit (ExitFailure 1)
