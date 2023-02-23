{-# OPTIONS_GHC -fwarn-tabs #-}

module Subst where

import qualified Data.List as List
import Data.Set

import Lambda

-- | Free variables of an expression
fvs :: Exp -> Set Var
fvs (Var x)     = singleton x
fvs (Lam x e)   = fvs e \\ singleton x
fvs (App e1 e2) = fvs e1 `union` fvs e2

-- | All variables. This is an infinite list!
allVars :: [Var]
allVars = [[c] | c <- ['a'..'z']] ++
          [cs++[c] | cs <- allVars, c <- ['a'..'z']]

-- | @subst v e1 e2@ substitutes @e1@ for @v@ in @e2@
subst :: Var -> Exp -> Exp -> Exp
subst x s t@(Var y)
    | y == x     = s
    | otherwise  = t

subst x s (Lam y t1)
    | y == x                      = Lam y  t1
    | y /= x && y `notElem` fvs s = Lam y  (subst x s t1)
    | otherwise                   = Lam y' (subst x s t1')
  where
    y'  = head (allVars List.\\ toList (insert x (fvs s `union` fvs t1)))
    t1' = subst y (Var y') t1

subst x s (App t1 t2) =
    App (subst x s t1) (subst x s t2)

-- | Call-by-name
cbn :: Exp -> Exp
cbn (App f x) =
    case f' of
      Lam v t -> cbn (subst v x t)
      _       -> App f' x
  where
    f' = cbn f

cbn e = e

-- | Call-by-value
cbv :: Exp -> Exp
cbv (App f x) =
      case f' of
         Lam v t -> cbv (subst v x' t)
         _       -> App f' x'
    where
        f' = cbv f
        x' = cbv x

cbv e = e
