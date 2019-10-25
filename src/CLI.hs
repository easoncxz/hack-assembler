
module CLI
  ( CliVersionType(..)
  , CliMode(..)
  , main
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Applicative
import Control.Monad

import qualified Automation.CabalVersion as CabalVersion
import Lib (assemble)

data CliVersionType
  = VersionAsGitTag
  | VersionAsCabalString

data CliMode
  = CliShowVersion CliVersionType
  | CliRunAssembler

main :: CliMode -> IO ()
main (CliShowVersion vtype) = do
  let format =
        case vtype of
          VersionAsGitTag -> CabalVersion.asTag
          VersionAsCabalString -> CabalVersion.asString
  v <- CabalVersion.readVersion
  putStrLn (format v)
main CliRunAssembler = do
  src <- map T.pack . lines <$> getContents
  case assemble src of
    Left error ->
      putStrLn . show $ error
    Right out ->
      mapM_ T.putStrLn out
