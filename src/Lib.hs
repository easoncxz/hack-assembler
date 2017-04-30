{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Control.Arrow ((***))
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import System.IO

import Model
import Parser (parseLine)
import Formatter (formatComp, formatAddr)

doStuff :: IO ()
doStuff = do
  assemble stdin stdout

-- | Run the assembler on the given source and output files.
assemble :: Handle -> Handle -> IO ()
assemble hSrc hOut = do
  lines <- map T.pack . lines <$> hGetContents hSrc :: IO [Text]
  let
    initState = withTable builtins
    finState = do
      symbolsState <- foldM collectLabels initState lines
      foldM produceOutput symbolsState lines
  case finState of
    Left e ->
      putStrLn . show $ e
    Right state@(ParseState { output }) ->
      mapM_ T.putStrLn output


-- | Parameterised for usage across the two passes.
-- |
-- | Line numbers refer to the lines *before* doing each step of
-- | srcLineNo is 1 because source-file line numbers are 1-indexed.
-- | outLineNo is 0 because instruction addresses are 0-indexed in the ROM.
withTable :: SymbolTable -> ParseState
withTable t = ParseState
  { srcLineNo = 1
  , outLineNo = 0
  , symbolTable = t
  , output = Seq.empty
  , nextVariableAddr = 16
  }

-- | Pre-defined symbols
builtins :: SymbolTable
builtins = Map.fromList $
  map (T.pack *** id) $
    [ ("SP", 0)
    , ("LCL", 1)
    , ("ARG", 2)
    , ("THIS", 3)
    , ("THAT", 4)
    , ("SCREEN", 16384)
    , ("KBD", 24576)
    ] ++ [ ("R" ++ show n, n) | n <- [0..15] ]

-- | Push the source-file line number forward
src :: ParseState -> ParseState
src state@(ParseState { srcLineNo }) =
  state { srcLineNo = srcLineNo + 1 }

-- | Push the output-file instruction index forward
out :: ParseState -> ParseState
out state@(ParseState { outLineNo }) =
  state { outLineNo = outLineNo + 1 }

-- | Add one line to the output
write :: Text -> ParseState -> ParseState
write out state@(ParseState { output }) =
  state { output = output |> out }

-- | Maybe update the ParseState's variable section according
-- | to the A-instruction's symbol.
addVariable :: AddrInstruction -> ParseState -> ParseState
addVariable (AddrLiteral {}) state = state  -- No symbol, no update.
addVariable
    (AddrSymbol sym)
    state@(ParseState { symbolTable, nextVariableAddr }) =
  case Map.lookup sym symbolTable of
    Nothing ->
      state
        { symbolTable = Map.insert sym nextVariableAddr symbolTable
        , nextVariableAddr = nextVariableAddr + 1
        }
    Just _ ->
      state   -- Idempotence is probably nice.

-- | foldM combinator
produceOutput :: ParseState -> Text -> Either AsmError ParseState
produceOutput
    state@(ParseState { srcLineNo, outLineNo, symbolTable, output, nextVariableAddr })
    line =
  case parseLine line of
    Just EmptyInstruction ->
      Right . src $ state
    Just (LabelInstruction {}) ->
      Right . src $ state
    Just (AddrInstruction a) ->
      let preventative@(ParseState { symbolTable }) = addVariable a state
      in case formatAddr symbolTable a of
        Just output ->
          Right . src . out . write output $ state
        Nothing ->
          Left $ ProgramError "Please report a bug: A-symbol bumping"
    Just (CompInstruction
        { instrDestination, instrComputation, instrAM, instrJump }) ->
      Right . src . out . write output $ state
        where
          output = formatComp
            instrDestination instrComputation instrAM instrJump
    Nothing ->
      Left $ SyntaxError srcLineNo "syntax error"

-- | foldM combinator
collectLabels :: ParseState -> Text -> Either AsmError ParseState
collectLabels
    state@(ParseState { srcLineNo, outLineNo, symbolTable, output, nextVariableAddr })
    line =
  case parseLine line of
    Just EmptyInstruction ->
      Right . src $ state
    Just (AddrInstruction {}) ->
      Right . src . out $ state   -- Do nothing in this pass
    Just (CompInstruction {}) ->
      Right . src . out $ state   -- Do nothing in this pass
    Just (LabelInstruction label) ->
      Right . src $ state
        { symbolTable = Map.insert label outLineNo symbolTable }
    Nothing ->
      Left $ SyntaxError srcLineNo "There's a syntax error on this line."
