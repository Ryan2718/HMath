{-|
Description : Unit Tests
Copyright   : (c) Ryan Forsyth, 2015
-}
module Main (main) where

import Test.HUnit
import Fraction
import Matrix

import Control.Monad (liftM)

fractionTests :: Test
fractionTests = test [
    "testToFraction1" ~: "" ~: "5/7" ~=? (show $ toFraction 5 7)
  , "testToFraction2" ~: "" ~: "-5/7" ~=? (show $ toFraction (-5) 7)
  , "testToFraction3" ~: "" ~: "-5/7" ~=? (show $ toFraction 5 (-7))
  , "testToFraction4" ~: "" ~: "5/7" ~=? (show $ toFraction (-5) (-7))
    
  , "test*" ~: "" ~: "15/28" ~=? (show $ (toFraction 3 7) * (toFraction 5 4))
  ]

a :: Maybe (Matrix Int)
a = fromList [[1,2,3], [4,5,6]] -- 2 x 3  matrix

b :: Maybe (Matrix Int)
b = fromList [[1,2], [3,4], [5,6]] -- 3 x 2 matrix

c :: Maybe (Matrix Int)
c = fromList [[]] -- 1 x 0

d :: Maybe (Matrix Int)
d = fromList [] -- 0 x 0

e :: Maybe (Matrix Int)
e = fromList [[1,2,3], [4,5]] -- Ragged Array

f :: Maybe (Matrix Int)
f = fromList [[], []] -- 2 x 0 

matrixTests :: Test
matrixTests = test [
      "testShow1" ~: "" ~: "Just \n[1,2,3]\n[4,5,6]\n" ~=? (show a)
    , "testShow2" ~: "" ~: "Just \n[1,2]\n[3,4]\n[5,6]\n" ~=? (show b)
    , "testShow3" ~: "" ~: "Nothing" ~=? (show c)
    , "testShow4" ~: "" ~: "Nothing" ~=? (show d)
    , "testShow5" ~: "" ~: "Nothing" ~=? (show e)
    , "testShow6" ~: "" ~: "Nothing" ~=? (show f)

    , "testNumRows1" ~: "" ~: Just 2 ~=? liftM numRows a
    , "testNumRows2" ~: "" ~: Just 3 ~=? liftM numRows b

    , "testNumCols1" ~: "" ~: Just 3 ~=? liftM numCols a
    , "testNumCols2" ~: "" ~: Just 2 ~=? liftM numCols b

    , "testDimensions1" ~: "" ~: Just (2,3) ~=? liftM dimensions a
    , "testDimensions2" ~: "" ~: Just (3,2) ~=? liftM dimensions b

    , "testGetElem1" ~: "" ~: Just 3 ~=? (a >>= getElem (1,3))
    , "testGetElem2" ~: "" ~: Nothing ~=? (a >>= getElem (4,5))

    , "testScalarMultiply1" ~: "" ~: "Just \n[3,6,9]\n[12,15,18]\n" ~=?
          (show $ liftM (scalarMultiply 3) a)
    , "testScalarMultiply2" ~: "" ~: "Just \n[3,6]\n[9,12]\n[15,18]\n" ~=?
          (show $ liftM (scalarMultiply 3) b)
    ]

{-
TODO:
Test all the matrix functions so far.
Implement add and multiply.
QuickCheck the Functor laws for the Matrix instance.
QuickCheck abs x * signum x == x
-- You may need to change abs and/or signum to get this to work
-}

main :: IO ()
main = do
  runTestTT fractionTests
  runTestTT matrixTests
  return ()

