{-# OPTIONS_GHC -Wall -Wno-type-defaults -fno-warn-missing-signatures #-}

module Examples where

import Prelude hiding (length, map, take, drop, sum, foldr, foldl)

--
-- List examples
--

-- Compute the length of the list l
length :: [a] -> Int
-- length xs = if null xs then 0 else 1 + length (tail xs)
length [] = 0
length (_:xs) = 1 + length xs


-- length xs = 1 + length (tail xs)

-- Compute the sum of a list of integers
sum :: Num a => [a] -> a
sum []    = 0
sum (x:xs) = x + sum xs

-- Return the nth element of a list, counting from 0.
nth :: Int -> [a] -> a
nth _ []     = error "nth: empty list"
nth 0 (x:_)  = x
nth n (_:xs) = nth (n-1) xs

-- Append two lists
append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

-- Take the first n elements of a list
take :: Int -> [a] -> [a]
take _ []     = []
take 0 _      = []
take n (x:xs) = x : take (n-1) xs

-- Drop the first n elements of a list
drop :: Int -> [a] -> [a]
drop _ []     = []
drop 0 xs     = xs
drop n (_:xs) = drop (n-1) xs

--
-- Partial application
--

-- Add two integers
add2 :: Int -> Int -> Int
add2 x y = x + y

-- Increment an integer
inc :: Int -> Int
inc x = x + 1
-- inc = \x -> x + 1
-- inc = (+) 1
-- inc = (1 +)
-- inc = (+ 1)

--
-- Higher-order functions
--

-- Increment all elements of a list by 1
incAll = error "incAll unimplemented"

-- Increment all elements of a list by a constant
addAll = error "addAll unimplemented"

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

-- Calculate the squares of a list of numbers. Make the function non-recursive.
-- squares xs = map (\x -> x*x) xs

{-
squares xs = map square xs
  where
    square x = x*x
-}

squares xs = let square x = x * x
             in
               map square xs

-- Now write squares using a list comprehension. We give this variant the name squares'
squares' xs = [x*x | x <- xs]

curry :: ((a,b) -> c) -> (a -> b -> c)
curry f = \x -> \y -> f (x, y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f = \(x, y) -> f x y
-- uncurry f = \args -> f (fst args) (snd args)

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

($) :: (a -> b) -> a -> b
f $ x = f x

-- What about papply?

--
-- Folds
--
foldr = error "foldr unimplemented"

foldl = error "foldl unimplemented"
