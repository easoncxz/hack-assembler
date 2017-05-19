#!/usr/bin/env stack runhaskell --package hack-assembler --

{-# LANGUAGE OverloadedStrings #-}

-- | Read the Cabal file and maybe create a new Git tag

import Prelude hiding (FilePath)
import qualified Control.Foldl as Fold
import Turtle
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Automation.CabalVersion (readVersion, asTag)

main = do
  tag <- T.pack . asTag <$> readVersion "hack-assembler.cabal"
  let matches = grep (text tag) (inshell "git tag -l" empty)
  count <- fold matches Fold.length
  case count of
    0 -> do
      proc "git" ["tag", tag] empty
      T.putStrLn $
        "[maybe-tag] Tagged for you: " `T.append` tag
    1 ->
      T.putStrLn $
        "[maybe-tag] The version didn't change: " `T.append` tag
    _ ->
      putStrLn "[maybe-tag] Something is funny with your versions!"

