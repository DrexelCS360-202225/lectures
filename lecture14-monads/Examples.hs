module Examples where

import Prelude hiding (Maybe(..), Either(..), map, mapM)

import System.Random

-- Defined as in Prelude
data Maybe a = Nothing
             | Just a
  deriving (Eq, Ord, Show, Read)

-- Defined as in Prelude
data Either a b = Left a
                | Right b
  deriving (Eq, Ord, Show, Read)

-- We can't hide the definition of the list data type, so we use an isomorphic
-- definition.
data List a = Nil
            | Cons a (List a)
  deriving (Eq, Ord, Show, Read)

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
  deriving (Eq, Ord, Show, Read)

map :: (a -> b) -> List a -> List b
map _ Nil         = Nil
map f (Cons x xs) = Cons (f x ) (map f xs)

--
-- Functors
--
instance Functor List where
    -- fmap :: (a -> b) -> List a -> List b
    fmap = map

instance Functor Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing  = Nothing
    fmap f (Just x) = Just (f x)

instance Functor Tree where
   -- map :: (a -> b) -> Tree a -> Tree b
   fmap f (Leaf x)   = Leaf (f x)
   fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Functor (Either a) where
    -- fmap :: (b -> c) -> Either a b -> Either a c
    fmap _ (Left x)  = Left x
    fmap f (Right x) = Right (f x)

--
-- Applicatives
--

instance Applicative Maybe where
    -- pure :: a -> Maybe a
    pure x = Just x

    -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing  <*> _ = Nothing
    (Just f) <*> x = fmap f x
{-
    (Just f) <*> Nothing  = Nothing
    (Just f) <*> (Just x) = Just (f x)
-}

{-
instance Applicative [] where
    -- pure :: a -> [x]
    pure x = [x]

    -- (<*>) :: [a -> b] -> [a] -> [b]
    gs <*> xs = [g x | g <- gs, x <- xs]
-}

--
-- Abstracting over computation
--

--
-- Total interpreter
--
data Exp = Const Int | Div Exp Exp

eval :: Exp -> Int
eval (Const n) = n
eval (Div x y) = eval x `div` eval y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval' :: Exp -> Maybe Int
eval' (Const n) = Just n
eval' (Div x y) = case eval' x of
                    Nothing -> Nothing
                    Just n  -> case eval' y of
                                 Nothing -> Nothing
                                 Just m  -> safediv n m

--
-- Total interpreter in monadic style
--

instance Monad Maybe where
  -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing >>= _ = Nothing
  Just x  >>= f = f x

eval'' :: Exp -> Maybe Int
eval'' (Const n) = Just n
eval'' (Div x y) = eval'' x >>= \n ->
                   eval'' y >>= \m ->
                   safediv n m

--
-- Functions that use random numbers
--

newtype Randomized a = R (StdGen -> (a, StdGen))

runR :: Randomized a -> StdGen -> (a, StdGen)
runR (R f) = f

instance Functor Randomized where
    -- fmap :: (a -> b) -> R a -> R b
    fmap f g = R $ \s ->
        let (x, s') = runR g s
        in
          (f x, s')

instance Applicative Randomized where
    pure x = R $ \s -> (x, s)

    mf <*> mx = R $ \s -> let (f, s')  = runR mf s
                              (x, s'') = runR mx s'
                          in
                            (f x, s'')

instance Monad Randomized where
    mx >>= f = R $ \s -> let (y, s') = runR mx s
                         in
                           runR (f y) s'

--
-- State Monad
--

newtype State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) = f

instance Functor (State s) where
    fmap f m = State $ \s -> let (x, s') = runState m s
                             in
                               (f x, s')

instance Applicative (State s) where
    pure x = State $ \s -> (x, s)

    mf <*> mx = State $ \s -> let (f, s')  = runState mf s
                                  (x, s'') = runState mx s'
                              in
                                (f x, s'')

instance Monad (State s) where
    mx >>= f = State $ \s -> let (x, s') = runState mx s
                             in
                               runState (f x) s'

--
-- Generic functions
--

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ []     = return []
mapM f (x:xs) = do y  <- f x
                   ys <- mapM f xs
                   return (y:ys)

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ []     = return []
filterM p (x:xs) = do b  <- p x
                      ys <- filterM p xs
                      return (if b then x:ys else ys)
