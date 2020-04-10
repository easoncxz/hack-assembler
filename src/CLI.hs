
module CLI
  ( CliVersionType(..)
  , CliMode(..)
  , main
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import Control.Applicative
import Control.Monad

import Lib (assemble)

import Paths_hack_assembler (version)

data CliVersionType
  = VersionAsGitTag
  | VersionAsCabalString

data CliMode
  = CliShowVersion CliVersionType
  | CliRunAssembler

main :: CliMode -> IO ()
main (CliShowVersion vtype) = do
  let format v =
        case vtype of
          VersionAsGitTag -> "v" ++ showVersion v
          VersionAsCabalString -> showVersion v
  putStrLn (format version)
main CliRunAssembler = do
  src <- map T.pack . lines <$> getContents
  case assemble src of
    Left error ->
      putStrLn . show $ error
    Right out ->
      mapM_ T.putStrLn out
