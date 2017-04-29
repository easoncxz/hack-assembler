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

data AsmParseState = AsmParseState
  { srcLineNo       :: SrcLineNo
  , outLineNo       :: OutLineNo
  , asmSymbolTable  :: AsmSymbolTable
  , asmOutput       :: AsmOutput
  }

data AsmError
  = SyntaxError SrcLineNo String
  deriving (Show)

reportError :: AsmError -> IO ()
reportError (SyntaxError (SrcLineNo ln) msg) =
  putStrLn $ "Error on line " ++ show ln ++ ": " ++ msg

initState :: AsmParseState
initState = tableState $ Map.fromList $
  [ (T.pack "SP"     , OutLineNo 0)
  , (T.pack "LCL"    , OutLineNo 1)
  , (T.pack "ARG"    , OutLineNo 2)
  , (T.pack "THIS"   , OutLineNo 3)
  , (T.pack "THAT"   , OutLineNo 4)
  , (T.pack "SCREEN" , OutLineNo 16384)
  , (T.pack "KBD"    , OutLineNo 24576)
  ] ++
  [(T.pack ("R" ++ show n), OutLineNo n) | n <- [0..15]]

tableState :: AsmSymbolTable -> AsmParseState
tableState t = AsmParseState
  { srcLineNo = SrcLineNo 1
  , outLineNo = OutLineNo 0
  , asmSymbolTable = t
  , asmOutput = []
  }

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
    (AsmParseState
      { srcLineNo = (SrcLineNo srcLineNo)
      , outLineNo = (OutLineNo outLineNo)
      , asmSymbolTable = table
      , asmOutput = output
      })
    (SrcLine line)
  | hasLabel line =
    case extractLabel line of
      Just label ->
        let table' = Map.insert label (OutLineNo outLineNo) table
        in Right (AsmParseState
          { srcLineNo = SrcLineNo (srcLineNo+1)
          , outLineNo = OutLineNo outLineNo
          , asmSymbolTable = table'
          , asmOutput = output
          })
      Nothing ->
        Left $ SyntaxError (SrcLineNo srcLineNo) "label syntax incorrect"
  | hasOutput line =
      Right (AsmParseState
        { srcLineNo = SrcLineNo (srcLineNo+1)
        , outLineNo = OutLineNo (outLineNo+1)
        , asmSymbolTable = table
        , asmOutput = output
        })
  | otherwise =
      Right (AsmParseState
        { srcLineNo = SrcLineNo (srcLineNo+1)
        , outLineNo = OutLineNo outLineNo
        , asmSymbolTable = table
        , asmOutput = output
        })



parseLine :: AsmParseState -> SrcLine -> Either AsmError AsmParseState
parseLine state (SrcLine srcLine) =
  Right state

runAssembler :: Handle -> Handle -> IO ()
runAssembler srcH outH = do
  srcLines <- map (SrcLine . T.pack) . lines <$> hGetContents srcH
  case foldM collectLabel initState srcLines of
    Right (AsmParseState { asmSymbolTable = t }) -> do
      putStrLn "Hello."
      putStrLn . show $ t   -- DEBUG
      case foldM parseLine (tableState t) srcLines of
        Right (AsmParseState { asmOutput = output }) ->
          putStrLn $ "assembly successful: " ++ show output
        Left e ->
          reportError e
    Left e ->
      putStrLn $ "There has been an error: " ++ show e

