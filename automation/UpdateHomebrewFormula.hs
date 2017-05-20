{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (FilePath)
import Control.Applicative
import Control.Lens
import Control.Monad.Managed
import qualified Control.Foldl as Fold
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Version
import System.Environment (getEnv, lookupEnv)
import qualified Filesystem.Path as Path
import Turtle
import Network.Wreq as Wreq
import Network.Wreq.Lens as Wreq

import Automation.Github (deployRelease, oauthToken)
import Automation.CabalVersion (readVersion)
import qualified Automation.CabalVersion as MyVersion

-- | Where is the sdist?
findSdist :: Version -> String
findSdist version =
  -- Let's use the .tar.gz built by Github --- it has the `stack.yaml`
  "https://github.com/easoncxz/hack-assembler/archive/v"
    ++ MyVersion.asString version
    ++ ".tar.gz"

-- | Get some bytes from the internet
downloadSdist :: String -> IO BL.ByteString
downloadSdist url = do
  resp <- Wreq.get url
  return $ resp ^. Wreq.responseBody

-- | Calculate checksum in memory
calcSha256 :: BL.ByteString -> Text
calcSha256 bytes =
  BB.lazyByteStringHex bytes
    & BB.toLazyByteString
    & BL.toStrict
    & T.decodeUtf8  -- unsafe

-- | We're running on Travis, yes?
isInTravis :: IO Bool
isInTravis = do
  t <- lookupEnv "TRAVIS"
  return (t /= Nothing)

gitConfig :: IO ()
gitConfig = do
  ExitSuccess <- shell "git config --global user.name \"Travis CI\"" empty
  ExitSuccess <- shell "git config --global user.email \"travis@travis-ci.org\"" empty
  return ()

-- | pushd you into a new repo, and cleanup afterwards
withGitClone :: Text -> IO () -> IO ()
withGitClone token inside = do
  let repoUrl = T.concat ["https://", token, "@github.com/easoncxz/homebrew-tap.git"]
  ExitSuccess <- proc "git" ["clone", repoUrl] empty
  ExitSuccess <- shell "git push" empty   -- verify credentials/authorizatin
  with (pushd "homebrew-tap") (\() -> inside)
  rm "homebrew-tap"

-- | Touch a Formula file in the homebrew-tap repo
updateFormula :: Text -> Text -> IO ()
updateFormula sdistUrl sha256 = do
  -- Assumes the CWD is in the homebrew-tap repo.
  let formulaPath = "Formula/hack-assembler.rb" :: FilePath
  -- Buffer the data into memory, since we need to clobber the file
  oldFormula <- fold (input formulaPath) Fold.list :: IO [Line]
  -- Let failures be before we clobber the file
  newFormula <- fold
    (inproc "../automation/update_formula_sdist.rb"
        [ "--source-tar-url", sdistUrl
        , "--source-tar-checksum", sha256
        ]
        (select oldFormula) :: Shell Line)
    Fold.list :: IO [Line]
  output formulaPath (select newFormula)

gitCommitAM :: IO ()
gitCommitAM = do
  buildNum <- fromMaybe "local-build" <$> lookupEnv "TRAVIS_BUILD_NUMBER"
  localCommitId <- fmap lineToText <$>
    fold (inshell "git rev-parse --short HEAD" empty) Fold.head
      :: IO (Maybe Text)
  commitIdMaybe <- lookupEnv "TRAVIS_COMMIT"
  let Just commitId = commitIdMaybe <|> fmap T.unpack localCommitId
  ExitSuccess <- proc "git"
    [ "commit"
    , "-am"
    , T.pack $
      "TravisCI/hack-assembler:"
        ++ " Build #" ++ buildNum
        ++ " (commit " ++ commitId ++ ")"
    ] empty
  return ()

gitPush :: IO ()
gitPush = do
  ExitSuccess <- shell "git push" empty
  return ()

main = do
  version <- readVersion "hack-assembler.cabal"
  token <- oauthToken
  inTravis <- isInTravis
  let sdistUrl = findSdist version :: String
  sdistBytes <- downloadSdist sdistUrl
  let shasum = calcSha256 sdistBytes
  when inTravis
    gitConfig
  withGitClone token $ do
    updateFormula (T.pack sdistUrl) shasum
    gitCommitAM
    putStrLn "About to push!"
    getLine
    gitPush

