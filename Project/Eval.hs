{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Eval (normalize) where

import Expr
import Subst

-- datatype of one-hole contexts for lambda expressions
data LExpCxt = Hole | A'1 LExpCxt LExp | A'2 LExp LExpCxt | L' Var LExpCxt
  deriving (Show, Eq)

-- we represent contexts "inside-out", i.e., with the parts of the
-- context nearest to the hole at the top-level.
-- The plugging function is defined accordingly.
plug :: LExpCxt -> LExp -> LExp
plug Hole d = d
plug (A'1 c e2) d = plug c (A d e2)
plug (A'2 e1 c) d = plug c (A e1 d)
plug (L' x c) d = plug c (L x d)

-- a pointer to a subexpression (a.k.a. "zipper") is a pair of a context and an expression
type LExpPtr = (LExpCxt, LExp)

-- a template for implementing normalize, as described in the
-- mini-project page...

example = L "x" (A (V "x") (V "y"))

example2 = A (L "x" (L "y" (A (V "y") (V "x")))) (L "z" (V "z"))

example3 = A (L "y" (A (V "y") (V "a"))) (A (L "x" (V "x")) (L "z" (A (L "u" (V "u")) (V "z"))))

theta = A (L "x" (L "y" (A (V "y") (A (A (V "x") (V "x")) (V "y"))))) (L "z" (L "t" (A (V "t") (A (A (V "z") (V "z")) (V "t")))))

y = L "f" (A (L "x" (A (V "f") (A (V "x") (V "x")))) (L "y" (A (V "f") (A (V "y") (V "y")))))

-- Exercises taken from https://www.cs.umd.edu/class/fall2017/cmsc330/tests/prac8-soln-fall13.pdf
-- expect a a
example4 = A (A (L "z" (V "z")) (L "y" (A (V "y") (V "y")))) (L "x" (A (V "x") (V "a")))

-- expect a a
example5 = A (A (L "x" (L "y" (A (A (V "x") (V "y")) (V "y")))) (L "a" (V "a"))) (V "b")

-- expect z t
example6 = A (L "x" (A (L "y" (A (V "x") (V "y"))) (V "t"))) (V "z")

-- expect \y. y
example7 = A (A (L "x" (A (V "x") (V "x"))) (L "y" (V "y"))) (L "z" (V "z"))

-- expect w
example8 = A (A (L "x" (L "y" (A (V "x") (V "y")))) (L "z" (V "z"))) (V "w")

subexp :: LExp -> [LExpPtr]
subexp (V v) = [(Hole, V v)]
subexp (A e1 e2) =
  ((Hole, A e1 e2) : foldr (\(c, expr) res -> (A'1 c e2, expr) : res) [] (subexp e1))
    ++ foldr (\(c, expr) res -> (A'2 e1 c, expr) : res) [] (subexp e2)
subexp (L v e) = (Hole, L v e) : foldr (\(c, expr) res -> (L' v c, expr) : res) [] (subexp e) --[(L' v Hole, e)]

filter_redex :: [LExpPtr] -> [LExpPtr]
filter_redex [] = []
filter_redex ((c, A (L v e1) e2) : xs) = (c, A (L v e1) e2) : filter_redex xs
filter_redex (_ : xs) = filter_redex xs

redex :: LExp -> [LExpPtr]
redex = filter_redex . subexp

stepBeta :: LExp -> LExp
stepBeta e =
  let beta_redices = redex e
   in if null beta_redices
        then e
        else
          let (c, A (L v e1) e2) = head beta_redices
           in plug c (subst (e2, v) e1)

normalize :: LExp -> LExp
normalize e =
  let res = stepBeta e
   in if res == e
        then res
        else normalize res
