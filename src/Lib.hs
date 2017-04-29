{-# LANGUAGE OverloadedStrings #-}
{- LANGUAGE FlexibleContexts #-}
{- LANGUAGE RankNTypes #-}
{- LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}


module Lib where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer.Lazy
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import System.IO


doStuff :: IO ()
doStuff =
  -- putStrLn $ "In `main`, we have done something."
  runAssembler stdin stdout

instance Show (a -> b) where
  show f = "<function>"

newtype SrcLine = SrcLine Text
newtype OutLine = OutLine Text
newtype SrcLineNo = SrcLineNo Int deriving (Show)
newtype OutLineNo = OutLineNo Int deriving (Show)

type AsmSymbolTable = Map.Map Text OutLineNo
type AsmOutput = [Text]
type AsmParseState = (SrcLineNo, OutLineNo, AsmSymbolTable, AsmOutput)

data AsmError
  = SyntaxError SrcLineNo String
  deriving (Show)

parseLine :: AsmParseState -> SrcLine -> Either AsmError AsmParseState
parseLine
    (SrcLineNo srcLineNo, OutLineNo outLineNo, table, output)
    (SrcLine srcLine) =
  Right (SrcLineNo srcLineNo, OutLineNo outLineNo, table, output)

reportError :: AsmError -> IO ()
reportError (SyntaxError (SrcLineNo ln) msg) =
  putStrLn $ "Error on line " ++ show ln ++ ": " ++ msg

initState :: AsmParseState
initState = tableState Map.empty

tableState :: AsmSymbolTable -> AsmParseState
tableState t = (SrcLineNo 1, OutLineNo 0, t, [])

runAssembler :: Handle -> Handle -> IO ()
runAssembler srcH outH = do
  srcLines <- map (SrcLine . T.pack) . lines <$> hGetContents srcH
  case foldM collectLabel initState srcLines of
    Right (_, _, t, _) -> do
      putStrLn "Hello."
      putStrLn . show $ t   -- DEBUG
      case foldM parseLine (tableState t) srcLines of
        Right (_, _, _, output) ->
          putStrLn $ "assembly successful: " ++ show output
        Left e ->
          reportError e
    Left e ->
      putStrLn $ "There has been an error: " ++ show e

stripComment :: Text -> Text
stripComment line =
  let (code, comment) = T.breakOn "//" line
  in T.strip code

-- | Whether the source line produces an output instruction
hasOutput :: Text -> Bool
hasOutput line = stripComment line /= "" && not (hasLabel line)

hasLabel :: Text -> Bool
hasLabel line = "(" `T.isInfixOf` line

extractLabel :: Text -> Maybe Text
extractLabel =
  T.stripSuffix ")" <=< T.stripPrefix "(" . stripComment

collectLabel :: AsmParseState -> SrcLine -> Either AsmError AsmParseState
collectLabel
    (SrcLineNo srcLineNo, OutLineNo outLineNo, table, output)
    (SrcLine line)
  | hasLabel line =
    case extractLabel line of
      Just label ->
        let table' = Map.insert label (OutLineNo outLineNo) table
        in Right (SrcLineNo (srcLineNo+1), OutLineNo outLineNo, table', output)
      Nothing ->
        Left $ SyntaxError (SrcLineNo srcLineNo) "label syntax incorrect"
  | hasOutput line =
      Right (SrcLineNo (srcLineNo+1), OutLineNo (outLineNo+1), table, output)
  | otherwise =
      Right (SrcLineNo (srcLineNo+1), OutLineNo outLineNo, table, output)


