{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Automation.CabalVersion
  ( readVersion , asTag , asString )
where

import Data.List
import Data.Version
import Data.Function ((&))
import Distribution.Verbosity (normal)
import Distribution.Package (pkgVersion)
import Distribution.PackageDescription (package, packageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)

-- | Read version from a Cabal file
readVersion :: FilePath -> IO Version
readVersion cabalFilePath =
  readPackageDescription normal cabalFilePath
    & fmap packageDescription
    & fmap package
    & fmap pkgVersion

-- | Suitable for using with Git
asTag :: Version -> String
asTag v = "v" ++ asString v

-- | Stack things don't mention "v"
asString :: Version -> String
asString (Version { versionBranch }) =
  concat $
    intersperse "." $
      map show $
        versionBranch
