{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Applicative
import Control.Monad
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Text as T
import Data.Text (Text)
import Text.Read


import qualified Model as Model

type SymbolTable = Map Text Int
type Output = Seq Text

data ParseState = ParseState
  { srcLineNo        :: Int
  , outLineNo        :: Int
  , symbolTable      :: SymbolTable
  , output           :: Output
  , nextVariableAddr :: Int
  }

data AsmError
  = SyntaxError Int String
  deriving (Show)

-- | Remove whitespace and comments
clean :: Text -> Text
clean line =
  let (code, comment) = T.breakOn "//" line
  in T.strip code

parse :: Text -> Maybe Model.HackInstruction
parse l = parseComp l <|> parseAddr l <|> parseLabel l

parseComp :: Text -> Maybe Model.HackInstruction
parseComp line = Nothing

parseAddr :: Text -> Maybe Model.HackInstruction
parseAddr = parse <=< T.stripPrefix "@" . clean
  where
    parse :: Text -> Maybe Model.HackInstruction
    parse l = fmap Model.AddrInstruction $
      literal l <|> symbol l

    literal :: Text -> Maybe Model.AddrInstruction
    literal = fmap Model.AddrLiteral . readMaybe . T.unpack

    symbol :: Text -> Maybe Model.AddrInstruction
    symbol = Just . Model.AddrSymbol

parseLabel :: Text -> Maybe Model.HackInstruction
parseLabel =
  (Just . Model.LabelInstruction)
  <=< T.stripSuffix ")"
  <=< T.stripPrefix "("
  . clean
