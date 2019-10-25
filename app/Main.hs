{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Applicative
import Control.Monad
import Data.Monoid ((<>))
import qualified Turtle

import Lib (assemble)
import qualified CLI

parseOptions :: IO CLI.CliMode
parseOptions = do
  command <- Turtle.options "Retrieve version from Cabal file" parser
  case command of
    Just "version-as-tag" -> return (CLI.CliShowVersion CLI.VersionAsGitTag)
    Just "version-as-string" -> return (CLI.CliShowVersion CLI.VersionAsCabalString)
    Just o -> fail ("Unrecognised command: " ++ show o)
    Nothing -> return CLI.CliRunAssembler
  where
    parser :: Turtle.Parser (Maybe Text)
    parser =
      Turtle.optional
        (Turtle.argText "COMMAND" "[ version-as-tag | version-as-string ]")

main :: IO ()
main = do
  mode <- parseOptions
  CLI.main mode
