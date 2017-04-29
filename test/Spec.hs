{-# LANGUAGE OverloadedStrings #-}

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
    (Just . Model.AddrInstruction . Model.AddrSymbol $ "LOOP")
  assertEqual "not a label at all"
    (Parser.parseLabel " @lol  // not a label")
    Nothing
  assertEqual "not a label either"
    (Parser.parseLabel "(adf  // syntax error")
    Nothing

main :: IO ()
main = do
  quickCheck reverseList
  runTestTT $ TestList
    [ TestLabel "clean" testClean
    , TestLabel "parse label" testParseLabel
    ]
  return ()
