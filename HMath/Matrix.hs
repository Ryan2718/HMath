{-|
Description : Tools for matrix manipulations
Copyright   : (c) Ryan Forsyth, 2015
-}
module Matrix
       (
         -- * Types
         Matrix

         -- * Construction
       , fromList

         -- * Details
       , numRows
       , numCols
       , dimensions
       , getElem

         -- * Manipulations
       , add
       , multiply
       , scalarMultiply  
       ) where

-- Types ----------------------------------------------------------------------

-- | Matrix Type
data Matrix n = Matrix { matrix :: [[n]] } deriving Eq

instance (Show n) => Show (Matrix n) where
  show mat =
    let strings = map (\sublist -> show sublist ++ "\n") (matrix mat)
    in "\n" ++ foldl (++) "" strings

instance Functor Matrix where
  fmap f a = Matrix $ fmap (fmap f) $ matrix a

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list =
  let (x, xs) = splitAt n list
  in x:(splitEvery n xs)

-- Construction ---------------------------------------------------------------

-- | Given a nested list, construct a Matrix
fromList :: (Num n) => [[n]] -> Maybe (Matrix n)
fromList [] = Nothing
fromList list@(x:_) =
  let m = length list -- Number of rows
      n = length x -- Number of columns
  in if (sameLength list) && (m >= 1) && (n >= 1)
     then Just $ Matrix list
     else Nothing

sameLength :: [[a]] -> Bool
sameLength [] = True
sameLength list@(x:_) = let lengths = map (\e -> length e) list
                        in lengths == replicate (length list) (length x)

-- Details --------------------------------------------------------------------

-- | The number of rows, m
numRows :: Matrix n -> Int
numRows = length . matrix
     
-- | The number of columns, n
numCols :: Matrix n -> Int
numCols mat =
  case matrix mat of
   [] -> 0
   x:xs -> length x

-- | Dimensions, m x n
dimensions :: Matrix n -> (Int, Int)
dimensions mat = (numRows mat, numCols mat)

-- | Access an element. Matrices are 1-indexed!
getElem :: (Int, Int) -> Matrix n -> Maybe n
getElem (i, j) mat = if (1 <= i) && (i <= numRows mat) &&
                        (1 <= j) && (j <= numCols mat)
                        then Just $ (matrix mat) !! (i - 1) !! (j - 1)
                        else Nothing

-- Manipulations --------------------------------------------------------------

-- | Add two matrices
add :: (Num n) => Matrix n -> Matrix n -> Maybe (Matrix n)
add a b = if dimensions a == dimensions b
          then let alist = matrix a
                   blist = matrix b
               in Just $ Matrix $ zipWith (zipWith (+)) alist blist
          else Nothing

-- | Multiply two matrices
multiply :: Matrix n -> Matrix n -> (Matrix n)
multiply a b = undefined

-- | Multiply a matrix by a scalar
scalarMultiply :: (Num n) => n -> Matrix n -> Matrix n
scalarMultiply c mat = fmap (c*) mat
