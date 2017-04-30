{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Control.Arrow ((***))
import Control.Applicative
import Control.Monad
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read

import Model

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

parseLine :: Text -> Maybe HackInstruction
parseLine l = parseEmpty l <|> parseLabel l <|> parseAddr l <|> parseComp l

-- | Remove whitespace and comments
clean :: Text -> Text
clean line =
  let (code, comment) = T.breakOn "//" line
  in T.strip code

parseEmpty :: Text -> Maybe HackInstruction
parseEmpty line = do
  guard (T.null (clean line))
  Just EmptyInstruction

parseLabel :: Text -> Maybe HackInstruction
parseLabel =
  (Just . LabelInstruction)
  <=< T.stripSuffix ")"
  <=< T.stripPrefix "("
  . clean

parseAddr :: Text -> Maybe HackInstruction
parseAddr = parse <=< T.stripPrefix "@" . clean
  where
    parse :: Text -> Maybe HackInstruction
    parse line = fmap AddrInstruction $
      literal line <|> symbol line

    literal :: Text -> Maybe AddrInstruction
    literal = fmap AddrLiteral . readMaybe . T.unpack

    symbol :: Text -> Maybe AddrInstruction
    symbol = Just . AddrSymbol

parseDest :: Text -> Maybe (Set HackRegister)
parseDest dest = do
  guard $ T.null $ T.filter (not . (`elem` ("DMA" :: String))) dest
  Just $ Set.fromList
    [r | r <- [RegD, RegM, RegA]
       , T.pack (show r) `T.isInfixOf` dest ]

parseExpr :: Text -> Maybe (HackComputation, HackAM)
parseExpr "1"  = Just ( ComputeOne    , UseA)
parseExpr "0"  = Just ( ComputeZero   , UseA)
parseExpr "-1" = Just ( ComputeNegOne , UseA)
parseExpr "D"  = Just ( ComputeD      , UseA)
parseExpr "M"  = Just ( ComputeR      , UseM)
parseExpr "A"  = Just ( ComputeR      , UseA)
parseExpr "!D" = Just ( NotD          , UseA)
parseExpr "!M" = Just ( NotR          , UseM)
parseExpr "!A" = Just ( NotR          , UseA)
parseExpr expr =
  if "+" `T.isInfixOf` expr then
    case map T.strip $ T.splitOn "+" expr of
      "D":"1":[] ->
        Just (DPlusOne, UseA)
      "A":"1":[] ->
        Just (RPlusOne, UseA)
      "M":"1":[] ->
        Just (RPlusOne, UseM)
      "D":"A":[] ->
        Just (DPlusR, UseA)
      "D":"M":[] ->
        Just (DPlusR, UseM)
      "A":"D":[] ->
        Just (DPlusR, UseA)
      "M":"D":[] ->
        Just (DPlusR, UseM)
      _ ->
        Nothing
  else if "-" `T.isInfixOf` expr then
    case map T.strip $ T.splitOn "-" expr of
      "D":"1":[] ->
        Just (DSubOne, UseA)
      "A":"1":[] ->
        Just (RSubOne, UseA)
      "M":"1":[] ->
        Just (RSubOne, UseM)
      "D":"A":[] ->
        Just (DSubR, UseA)
      "D":"M":[] ->
        Just (DSubR, UseM)
      "A":"D":[] ->
        Just (RSubD, UseA)
      "M":"D":[] ->
        Just (RSubD, UseM)
      _ ->
        Nothing
  else if "&" `T.isInfixOf` expr then
    case map T.strip $ T.splitOn "&" expr of
      "D":"A":[] ->
        Just (DAndR, UseA)
      "D":"M":[] ->
        Just (DAndR, UseM)
      "A":"D":[] ->
        Just (DAndR, UseA)
      "M":"D":[] ->
        Just (DAndR, UseM)
      _ ->
        Nothing
  else if "|" `T.isInfixOf` expr then
    case map T.strip $ T.splitOn "&" expr of
      "D":"A":[] ->
        Just (DOrR, UseA)
      "D":"M":[] ->
        Just (DOrR, UseM)
      "A":"D":[] ->
        Just (DOrR, UseA)
      "M":"D":[] ->
        Just (DOrR, UseM)
      _ ->
        Nothing
  else
    Nothing

parseJump :: Text -> Maybe (Set Ordering)
parseJump "" = Just (Set.empty)
parseJump "null" = Just (Set.empty)
parseJump "JGT" = Just (Set.fromList [GT])
parseJump "JGE" = Just (Set.fromList [GT, EQ])
parseJump "JLT" = Just (Set.fromList [LT])
parseJump "JLE" = Just (Set.fromList [LT, EQ])
parseJump "JEQ" = Just (Set.fromList [EQ])
parseJump "JNE" = Just (Set.fromList [LT, GT])
parseJump "JMP" = Just (Set.fromList [LT, EQ, GT])
parseJump _ = Nothing

splitComp :: Text -> (Text, Text, Text)
splitComp line =
  let
    (destAndComp, jump) =
      (T.strip *** (T.strip . T.dropWhile (== ';')))
        (T.breakOn ";" line)
    (rComp, rDest) =
      (T.strip *** (T.strip . T.dropWhile (== '=')))
        (T.breakOn "=" (T.reverse destAndComp))
    (dest, comp) =
      (T.reverse *** T.reverse) (rDest, rComp)
  in (dest, comp, jump)

parseComp :: Text -> Maybe HackInstruction
parseComp line = do
  let (dest, expr, jump) = splitComp (clean line)
  d <- parseDest dest
  j <- parseJump jump
  (c, am) <- parseExpr expr
  return $ CompInstruction d c am j
