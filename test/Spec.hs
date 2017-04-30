{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Test.QuickCheck
import Test.HUnit

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Lib
import qualified Model as Model
import qualified Parser as Parser
import qualified Formatter as Formatter

reverseList :: [Int] -> Bool
reverseList xs =
  xs == reverse (reverse xs)

testClean :: Test
testClean = TestCase $ do
  assertEqual "" (Parser.clean "abc   // adsfasdf") "abc"
  assertEqual "" (Parser.clean "   abc    ") "abc"

testParseLabel :: Test
testParseLabel = TestCase $ do
  assertEqual "really a label"
    (Parser.parseLabel "  (LOOP)   // start here")
    (Just . Model.LabelInstruction $ "LOOP")
  assertEqual "not a label at all"
    (Parser.parseLabel " @lol  // not a label")
    Nothing
  assertEqual "not a label either"
    (Parser.parseLabel "(adf  // syntax error")
    Nothing

testParseAddress :: Test
testParseAddress = TestCase $ do
  assertEqual "a literal address"
    (Parser.parseAddr "@1234  // lol")
    (Just . Model.AddrInstruction . Model.AddrLiteral $ 1234)
  assertEqual "a symbolic address"
    (Parser.parseAddr "  @sum")
    (Just . Model.AddrInstruction . Model.AddrSymbol $ "sum")
  forM_
    [ Parser.parseAddr "   D=A"
    , Parser.parseAddr " // nothing here"
    , Parser.parseAddr "(LOOP)"
    ] $ \c ->
      assertEqual "not an address" c Nothing

testFormatting :: Test
testFormatting = TestCase $ do
  assertEqual "C-instruction expression"
    (Formatter.formatCompExpr Model.DPlusOne)
    "011111"
  assertEqual "C-instruction destination"
    (Formatter.formatDest $ Set.fromList [Model.RegA, Model.RegM])
    "101"
  assertEqual "C-instruction destination (2)"
    (Formatter.formatDest $ Set.singleton Model.RegA)
    "100"
  assertEqual "C-instruction jump"
    (Formatter.formatJump $ Set.fromList [EQ, GT])
    "011"
  assertEqual "C-instruction jump (2)"
    (Formatter.formatJump $ Set.fromList [GT])
    "001"
  assertEqual "A-instruction"
    (Formatter.formatInstruction
      Map.empty
      (Model.AddrInstruction
        (Model.AddrLiteral 5)))
    (Just "0000000000000101")
  assertEqual "Whole C-instruction"
    (Formatter.formatInstruction Map.empty $
      Model.CompInstruction
        { Model.instrDestination = (Set.fromList [Model.RegA, Model.RegD])
        , Model.instrComputation = Model.RSubOne
        , Model.instrAM = Model.UseM
        , Model.instrJump = (Set.fromList [GT,EQ])
        })
    (Just "1111110010110011")


main :: IO ()
main = do
  quickCheck reverseList
  runTestTT $ TestList
    [ TestLabel "clean" testClean
    , TestLabel "parse label" testParseLabel
    , TestLabel "parse address" testParseAddress
    , TestLabel "format instructions" testFormatting
    ]
  return ()
