{-# LANGUAGE FlexibleInstances #-}
{-|
Description : Tools for working with fractions
Copyright   : (c) Ryan Forsyth, 2015
-}
module Fraction
       (
         -- * Types
         Fraction
         -- * Functions
       , toFraction
       )
       where

-- | Fraction data type. Requires a numerator n and denominator d
data Fraction n d = Fraction n d deriving Eq

-- | Given two numbers, create a Fraction
toFraction :: Int -> Int -> Fraction Int Int
toFraction n d =
  -- The numerator holds the negative sign, if there is one
  let d' = if d < 0 then -1 * d else d
      n' = if d < 0 then -1 * n else n
      factor = gcd n' d'
  in Fraction (quot n' factor) (quot d' factor)

instance (Show n, Show d) => Show (Fraction n d) where
  show (Fraction n d) = (show n) ++ "/" ++ (show d)
  
instance (Num n) => Num (Fraction n n) where
  (+) (Fraction a b) (Fraction c d) = Fraction (a * d + b * c) (b * d)
  (*) (Fraction a b) (Fraction c d) = Fraction (a * c) (b * d)
  abs (Fraction a b) = Fraction (abs a) (abs b)
  signum (Fraction a b) = Fraction (signum a) b -- b > 0 always
  fromInteger a = Fraction (fromInteger a) (fromInteger 1) -- Divide by 1
  negate (Fraction a b) = Fraction (-1 * a) b
