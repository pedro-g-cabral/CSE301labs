module Expr where

import Data.List

type Var = String
data LExp = V Var | A LExp LExp | L Var LExp
  deriving (Show,Read)

-- it is sometimes useful to classify expressions
-- by their top-level constructor
isVar, isApp, isLam :: LExp -> Bool
isVar (V _)   = True
isVar _       = False
isApp (A _ _) = True
isApp _       = False
isLam (L _ _) = True
isLam _       = False

-- compute the list of free vars of an expression,
-- in left-to-right order
free :: LExp -> [Var]
-- if the expression is a variable, then it is free
free (V x)     = [x]
-- if the expression is an application, then the free variables
-- are the union of the free variables of the two subexpressions
free (A t1 t2) = free t1 `union` free t2
-- if the expression is a lambda, then the free variables
-- are the free variables of the body, minus the bound variable
free (L x t1)  = free t1 \\ [x]

-- apply a variable renaming
rename :: (Var -> Var) -> LExp -> LExp
rename f (V x)     = V (f x)
rename f (A t1 t2) = A (rename f t1) (rename f t2)
rename f (L x t1)  = L (f x) (rename f t1)

-- apply a variable swapping
swapname :: (Var,Var) -> LExp -> LExp
swapname (x,y) = rename (\z -> if z == x then y else if z == y then x else z)

-- test for alpha-equivalence
alphaEq :: LExp -> LExp -> Bool
alphaEq (V x1)    (V x2)    = x1 == x2
alphaEq (A t1 u1) (A t2 u2) = alphaEq t1 t2 && alphaEq u1 u2
-- alpha-equivalence of lambda expressions is a bit more complicated
-- because we need to make sure that the bound variables are distinct
alphaEq (L x1 t1) (L x2 t2) = alphaEq t1 (swapname (x2,x1) t2)
alphaEq _         _         = False

-- equality of lambda expressions is given by alpha-equivalence
instance Eq LExp where
  t1 == t2 = alphaEq t1 t2

