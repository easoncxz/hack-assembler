{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck

import Lib

reverseList :: [Int] -> Bool
reverseList xs =
  xs == reverse (reverse xs)

prop_stripComment :: Bool
prop_stripComment =
  stripComment "  // comment" == ""

main :: IO ()
main = do
  quickCheck reverseList
  quickCheck prop_stripComment
