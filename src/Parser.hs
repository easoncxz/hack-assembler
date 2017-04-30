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
parse l = parseEmpty l <|> parseLabel l <|> parseAddr l <|> parseComp l

parseEmpty :: Text -> Maybe Model.HackInstruction
parseEmpty line = do
  guard (T.null (clean line))
  Just Model.EmptyInstruction

parseLabel :: Text -> Maybe Model.HackInstruction
parseLabel =
  (Just . Model.LabelInstruction)
  <=< T.stripSuffix ")"
  <=< T.stripPrefix "("
  . clean

parseAddr :: Text -> Maybe Model.HackInstruction
parseAddr = parse <=< T.stripPrefix "@" . clean
  where
    parse :: Text -> Maybe Model.HackInstruction
    parse line = fmap Model.AddrInstruction $
      literal line <|> symbol line

    literal :: Text -> Maybe Model.AddrInstruction
    literal = fmap Model.AddrLiteral . readMaybe . T.unpack

    symbol :: Text -> Maybe Model.AddrInstruction
    symbol = Just . Model.AddrSymbol

parseDest :: Text -> Maybe (Set Model.HackRegister)
parseDest dest = do
  guard $ T.null $ T.filter (not . (`elem` ("DMA" :: String))) dest
  Just $ Set.fromList
    [r | r <- [Model.RegD, Model.RegM, Model.RegA]
       , T.pack (show r) `T.isInfixOf` dest ]

parseExpr :: Text -> Maybe (Model.HackComputation, Model.HackAM)
parseExpr "1"  = Just ( Model.ComputeOne    , Model.UseA)
parseExpr "0"  = Just ( Model.ComputeZero   , Model.UseA)
parseExpr "-1" = Just ( Model.ComputeNegOne , Model.UseA)
parseExpr "D"  = Just ( Model.ComputeD      , Model.UseA)
parseExpr "M"  = Just ( Model.ComputeR      , Model.UseM)
parseExpr "A"  = Just ( Model.ComputeR      , Model.UseA)
parseExpr "!D" = Just ( Model.NotD          , Model.UseA)
parseExpr "!M" = Just ( Model.NotR          , Model.UseM)
parseExpr "!A" = Just ( Model.NotR          , Model.UseA)
parseExpr expr =
  if "+" `T.isInfixOf` expr then
    case map T.strip $ T.splitOn "+" expr of
      "D":"1":[] ->
        Just (Model.DPlusOne, Model.UseA)
      "A":"1":[] ->
        Just (Model.RPlusOne, Model.UseA)
      "M":"1":[] ->
        Just (Model.RPlusOne, Model.UseM)
      "D":"A":[] ->
        Just (Model.DPlusR, Model.UseA)
      "D":"M":[] ->
        Just (Model.DPlusR, Model.UseM)
      "A":"D":[] ->
        Just (Model.DPlusR, Model.UseA)
      "M":"D":[] ->
        Just (Model.DPlusR, Model.UseM)
      _ ->
        Nothing
  else if "-" `T.isInfixOf` expr then
    case map T.strip $ T.splitOn "-" expr of
      "D":"1":[] ->
        Just (Model.DSubOne, Model.UseA)
      "A":"1":[] ->
        Just (Model.RSubOne, Model.UseA)
      "M":"1":[] ->
        Just (Model.RSubOne, Model.UseM)
      "D":"A":[] ->
        Just (Model.DSubR, Model.UseA)
      "D":"M":[] ->
        Just (Model.DSubR, Model.UseM)
      "A":"D":[] ->
        Just (Model.RSubD, Model.UseA)
      "M":"D":[] ->
        Just (Model.RSubD, Model.UseM)
      _ ->
        Nothing
  else if "&" `T.isInfixOf` expr then
    case map T.strip $ T.splitOn "&" expr of
      "D":"A":[] ->
        Just (Model.DAndR, Model.UseA)
      "D":"M":[] ->
        Just (Model.DAndR, Model.UseM)
      "A":"D":[] ->
        Just (Model.DAndR, Model.UseA)
      "M":"D":[] ->
        Just (Model.DAndR, Model.UseM)
      _ ->
        Nothing
  else if "|" `T.isInfixOf` expr then
    case map T.strip $ T.splitOn "&" expr of
      "D":"A":[] ->
        Just (Model.DOrR, Model.UseA)
      "D":"M":[] ->
        Just (Model.DOrR, Model.UseM)
      "A":"D":[] ->
        Just (Model.DOrR, Model.UseA)
      "M":"D":[] ->
        Just (Model.DOrR, Model.UseM)
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

parseComp :: Text -> Maybe Model.HackInstruction
parseComp line = do
  let (dest, expr, jump) = splitComp (clean line)
  d <- parseDest dest
  j <- parseJump jump
  (c, am) <- parseExpr expr
  return $ Model.CompInstruction d c am j
