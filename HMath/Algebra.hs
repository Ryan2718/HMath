{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Description : Tools for Abstract Algebra
Copyright   : (c) Ryan Forsyth, 2015
-}
module Algebra where

-- Type Classes ----------------------------------------------------------------


-- | Defines a Group.
-- = Group Laws (See Wikipedia Article)
-- (1) Closure: for a,b in G, a # b in G
-- (2) Associativity: for a,b,c in G, (a # b) # c == a # (b # c)
-- (3) Identity element: There exists e in G such that
--     e # a == a # e == a for all a in G
-- (4) Inverse element: for a in G, there exists b in G such that
--     a # b == b # a == e
class Group a where
  -- | The group operation
  (#) :: a -> a -> a
  
  -- | The inverse of the group operation
  (/#) :: a -> a -> a
  
  -- | The identity element of the group
  identity :: a
  
  -- | The inverse of an element in the group
  inverse :: a -> a
  inverse a = identity /# a

instance (Num n) => Group n where
  (#) = (+)
  (/#) = (-)
  identity = 0

{-
-- | Defines an Abelian Group. Adds a law to Groups:
-- Commutativity: for a,v in G, a # b = b # a
class (Group a) => AbelianGroup a where
-- No functions?
-}
