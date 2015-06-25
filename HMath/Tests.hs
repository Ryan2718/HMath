{-|
Description : Unit Tests
Copyright   : (c) Ryan Forsyth, 2015
-}
module Main (main) where

import Test.HUnit
import Fraction

tests :: Test
tests = test [
    "testToFraction1" ~: "" ~: "5/7" ~=? (show $ toFraction 5 7)
  , "testToFraction2" ~: "" ~: "-5/7" ~=? (show $ toFraction (-5) 7)
  , "testToFraction3" ~: "" ~: "-5/7" ~=? (show $ toFraction 5 (-7))
  , "testToFraction4" ~: "" ~: "5/7" ~=? (show $ toFraction (-5) (-7))
  , "test*" ~: "" ~: "15/28" ~=? (show $ (toFraction 3 7) * (toFraction 5 4))
  ]

main :: IO ()
main = do
  runTestTT tests
  return ()

