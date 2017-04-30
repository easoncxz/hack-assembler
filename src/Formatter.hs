{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Formatter where

import Data.Char (intToDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Numeric (showIntAtBase)

import Model

-- | Pad short strings; leave too-long/long-enough strings alone
leftpad :: Int -> Char -> String -> String
leftpad l c s = replicate (max 0 (l - length s)) c ++ s

-- | Converts any (pos/neg) integer to a 16-bit bin-string
toBinaryString :: Int -> Text
toBinaryString n =
  T.pack $
    leftpad 16 '0' $
      showIntAtBase 2 intToDigit (n `mod` 2 ^ 15) ""

formatCompExpr :: HackComputation -> Text
formatCompExpr expr =
  T.pack $ case expr of
    ComputeZero   -> "101010"
    ComputeOne    -> "111111"
    ComputeNegOne -> "111010"
    ComputeD      -> "001100"
    ComputeR      -> "110000"
    NotD          -> "001101"
    NotR          -> "110001"
    NegD          -> "001111"
    NegR          -> "110011"
    DPlusOne      -> "011111"
    RPlusOne      -> "110111"
    DSubOne       -> "001110"
    RSubOne       -> "110010"
    DPlusR        -> "000010"
    DSubR         -> "010011"
    RSubD         -> "000111"
    DAndR         -> "000000"
    DOrR          -> "010101"

formatDest :: Set (HackRegister) -> Text
formatDest dest =
  T.pack $ map hot [RegA, RegD, RegM]
  where
    hot :: HackRegister -> Char
    hot reg =
      if Set.member reg dest then '1' else '0'

formatJump :: Set (Ordering) -> Text
formatJump jump =
  T.pack $ map hot [LT, EQ, GT]
  where
    hot :: Ordering -> Char
    hot o =
      if Set.member o jump then '1' else '0'

formatAM :: HackAM -> Text
formatAM UseA = "0"
formatAM UseM = "1"

-- | We can always produce output
formatComp ::
  Set HackRegister -> HackComputation -> HackAM -> Set Ordering -> Text
formatComp dest comp am jump =
  T.concat
    [ "111"
    , formatAM am
    , formatCompExpr comp
    , formatDest dest
    , formatJump jump
    ]

-- | We may fail due to symbol-lookup miss
formatAddr :: SymbolTable -> AddrInstruction -> Maybe Text
formatAddr table (AddrLiteral lit) =
  Just (toBinaryString lit)
formatAddr table (AddrSymbol sym) = do
  lit <- Map.lookup sym table
  return (toBinaryString lit)
