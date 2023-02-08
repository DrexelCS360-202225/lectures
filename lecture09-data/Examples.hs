{-# OPTIONS_GHC -Wall #-}

module Examples where

import Prelude hiding (map, take, drop)

--
-- Inductive definitions of natural numbers
--

data Nat = Zero | Succ Nat

-- isNat :: Nat -> Bool

nat2int :: Nat -> Int
nat2int = error "nat2int: not yet defined"

int2nat :: Int -> Nat
int2nat = error "int2nat: not yet defined"

addCheat :: Nat -> Nat -> Nat
addCheat m n = int2nat (nat2int m + nat2int n)

add :: Nat -> Nat -> Nat
add = error "add: not yet defined"

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
