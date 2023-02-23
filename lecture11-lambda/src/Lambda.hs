{-# OPTIONS_GHC -fwarn-tabs #-}

module Lambda (
  Var,
  Exp(..)
) where

type Var = String

data Exp = Var Var
         | Lam Var Exp
         | App Exp Exp
  deriving (Eq, Show)
