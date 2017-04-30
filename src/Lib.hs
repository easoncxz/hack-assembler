{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Control.Arrow ((***))
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Model
import Parser (parseLine)

doStuff :: IO ()
doStuff = do
  lines <- map T.pack . lines <$> getContents
  let
    s = parseStateWithTable builtinSymbols
    result = foldM collectLabels s lines
  putStrLn . show $ result
builtinSymbols :: SymbolTable
builtinSymbols = Map.fromList $
  map (T.pack *** id) $
    [ ("SP", 0)
    , ("LCL", 1)
    , ("ARG", 2)
    , ("THIS", 3)
    , ("THAT", 4)
    , ("SCREEN", 16384)
    , ("KBD", 24576)
    ] ++ [ ("R" ++ show n, n) | n <- [0..15] ]

parseStateWithTable :: SymbolTable -> ParseState
parseStateWithTable table = ParseState
  { srcLineNo = 1
  , outLineNo = 0
  , symbolTable = table
  , output = Seq.empty
  , nextVariableAddr = 16
  }

collectLabels :: ParseState -> Text -> Either AsmError ParseState
collectLabels
    state@(ParseState { srcLineNo, outLineNo, symbolTable, output, nextVariableAddr })
    line =
  case parseLine line of
    Just (LabelInstruction label) ->
      Right $ state
        { srcLineNo = srcLineNo + 1
        , symbolTable = Map.insert label outLineNo symbolTable
        }
    Just EmptyInstruction ->
      Right $ state { srcLineNo = srcLineNo + 1 }
    Just (AddrInstruction _) ->
      Right $ state
        { srcLineNo = srcLineNo + 1
        , outLineNo = outLineNo + 1
        }
    Just (CompInstruction {}) ->
      Right $ state
        { srcLineNo = srcLineNo + 1
        , outLineNo = outLineNo + 1
        }
    Nothing ->
      Left $ SyntaxError srcLineNo "There's a syntax error on this line."


