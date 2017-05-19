{-# LANGUAGE OverloadedStrings #-}

-- | Deploy sdist tarballs onto Github as a Github Release

import Prelude hiding (FilePath)
import Control.Applicative

import Turtle
import Turtle.Options
import Data.Optional
import Filesystem.Path.CurrentOS (encodeString)

import Automation.Github (deployRelease)
import Automation.CabalVersion (readVersion)

parser :: Parser FilePath
parser = argPath "cabal-file" (Specific "The path to your project's Cabal file")

main = do
  cabalPath <- encodeString <$> options "hack-assembler deployer" parser
  version <- readVersion cabalPath
  deployRelease version

