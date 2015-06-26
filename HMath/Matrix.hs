{-|
Description : Tools for matrix manipulations
Copyright   : (c) Ryan Forsyth, 2015
-}
module Matrix
       (
         -- * Types
         Matrix

         -- * Construction
       , fromArray
       , fromList

         -- * Details
       , numRows
       , numCols

         -- * Manipulations
       , add
       , multiply
       , scalarMultiply  
       ) where

import Data.Array (Array(..), array, bounds, elems, listArray)

-- Types ----------------------------------------------------------------------

-- | Matrix Type
data Matrix n = Matrix { matrix :: Array (Int, Int) n } deriving Eq

instance (Show n) => Show (Matrix n) where
  show mat =
    let m = numRows mat
        n = numCols mat
        list = elems $ matrix mat
        nested = splitEvery n list
        strings = map (\sublist -> show sublist ++ "\n") nested
    in "\n" ++ foldl (++) "" strings

instance (Num n) => Num (Matrix n) where
  (+) a b = add a b
  (*) a b = multiply a b
  abs a = fmap abs a -- absolute value of every entry
  signum a = fmap signum a -- signum of every entry
  fromInteger a = Matrix $ array ((1,1), (1,1)) [((1,1), (fromInteger a))]
                  -- 1 x 1 matrix
  negate a = fmap ((-1)*) a -- negate every entry

instance Functor Matrix where
  fmap f a = Matrix $ fmap f $ matrix a

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list =
  let (x, xs) = splitAt n list
  in x:(splitEvery n xs)

-- Construction ---------------------------------------------------------------
     
-- | Given a two-dimensional array, construct a Matrix
fromArray :: (Num n) => Array (Int, Int) n -> Maybe (Matrix n)
fromArray a = let (lower, upper) = bounds a
              in if lower == (1,1)
                 then Just $ Matrix a
                 else Nothing     

-- | Given a nested list, construct a Matrix
fromList :: (Num n) => [[n]] -> Maybe (Matrix n)
fromList [] = Nothing
fromList [[]] = Nothing
fromList list@(x:_) =
  let m = length list -- Number of rows
      n = length x -- Number of columns
  in Just $ Matrix $ listArray ((1, 1), (m, n)) $ concat list

-- Details --------------------------------------------------------------------

-- | The number of rows, m
numRows :: Matrix n -> Int
numRows mat =
  let ((_,_), (m,_)) = bounds $ matrix mat
  in m
     
-- | The number of columns, n
numCols :: Matrix n -> Int
numCols mat =
  let ((_,_), (_,n)) = bounds $ matrix mat
  in n

-- Manipulations --------------------------------------------------------------

-- | Add two matrices
add :: Matrix n -> Matrix n -> Matrix n
add a b = undefined

-- | Multiply two matrices
multiply :: Matrix n -> Matrix n -> Matrix n
multiply a b = undefined

-- | Multiply a matrix by a scalar
scalarMultiply :: (Num n) => n -> Matrix n -> Matrix n
scalarMultiply c mat = fmap (c*) mat
