{-# OPTIONS_GHC -Wall #-}

module Examples where

import Prelude hiding (map, take, drop)

--
-- Inductive definitions of natural numbers
--

data Nat = Zero | Succ Nat
  deriving (Show)

-- isNat :: Nat -> Bool

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

addCheat :: Nat -> Nat -> Nat
addCheat m n = int2nat (nat2int m + nat2int n)

add :: Nat -> Nat -> Nat
add Zero               n = n
add (Succ (Succ Zero)) n = Succ (Succ n)
add (Succ m)           n = Succ (add m n)

--
-- Inductive definitions of lists
--

data List a = Nil
            | Cons a (List a)

--
-- Define a safe head function
--

data MaybeHead = NoHead
               | JustHead Int

safeHeadInts = undefined

safeHead = undefined

--
-- Type classes
--

data Foo = F Int | G Char

-- instance Eq Foo where
