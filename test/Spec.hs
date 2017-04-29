{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Test.QuickCheck
import Test.HUnit

import Lib
import qualified Model as Model
import qualified Parser as Parser

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


main :: IO ()
main = do
  quickCheck reverseList
  runTestTT $ TestList
    [ TestLabel "clean" testClean
    , TestLabel "parse label" testParseLabel
    , TestLabel "parse address" testParseAddress
    ]
  return ()
