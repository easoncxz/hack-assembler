{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Test.HUnit
import Test.QuickCheck

import qualified Formatter as Formatter
import Lib
import qualified Model as Model
import qualified Parser as Parser

reverseList :: [Int] -> Bool
reverseList xs = xs == reverse (reverse xs)

testClean :: Test
testClean =
  TestCase $ do
    assertEqual "" "abc" (Parser.clean "abc   // adsfasdf")
    assertEqual "" "abc" (Parser.clean "   abc    ")

testParseLabel :: Test
testParseLabel =
  TestCase $ do
    assertEqual
      "really a label"
      (Just . Model.LabelInstruction $ "LOOP")
      (Parser.parseLabel "  (LOOP)   // start here")
    assertEqual
      "not a label at all"
      Nothing
      (Parser.parseLabel " @lol  // not a label")
    assertEqual
      "not a label either"
      Nothing
      (Parser.parseLabel "(adf  // syntax error")

testParseAddress :: Test
testParseAddress =
  TestCase $ do
    assertEqual
      "a literal address"
      (Just . Model.AddrInstruction . Model.AddrLiteral $ 1234)
      (Parser.parseAddr "@1234  // lol")
    assertEqual
      "a symbolic address"
      (Just . Model.AddrInstruction . Model.AddrSymbol $ "sum")
      (Parser.parseAddr "  @sum")
    forM_
      [ Parser.parseAddr "   D=A"
      , Parser.parseAddr " // nothing here"
      , Parser.parseAddr "(LOOP)"
      ] $ \c -> assertEqual "not an address" Nothing c

testFormatting :: Test
testFormatting =
  TestCase $ do
    assertEqual
      "C-instruction expression"
      "011111"
      (Formatter.formatCompExpr Model.DPlusOne)
    assertEqual
      "C-instruction destination"
      "101"
      (Formatter.formatDest $ Set.fromList [Model.RegA, Model.RegM])
    assertEqual
      "C-instruction destination (2)"
      "100"
      (Formatter.formatDest $ Set.singleton Model.RegA)
    assertEqual
      "C-instruction jump"
      "011"
      (Formatter.formatJump $ Set.fromList [EQ, GT])
    assertEqual
      "C-instruction jump (2)"
      "001"
      (Formatter.formatJump $ Set.fromList [GT])
    assertEqual
      "A-instruction"
      (Just "0000000000000101")
      (Formatter.formatAddr Map.empty (Model.AddrLiteral 5))
    assertEqual
      "Whole C-instruction"
      "1111110010110011"
      (Formatter.formatComp
         (Set.fromList [Model.RegA, Model.RegD])
         Model.RSubOne
         Model.UseM
         (Set.fromList [GT, EQ]))

main :: IO ()
main = do
  quickCheck reverseList
  runTestTT $
    TestList
      [ TestLabel "clean" testClean
      , TestLabel "parse label" testParseLabel
      , TestLabel "parse address" testParseAddress
      , TestLabel "format instructions" testFormatting
      ]
  return ()
