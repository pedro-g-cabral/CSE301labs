module Subst (subst) where

-- import Var from Expr.hs
import Expr

fresh :: [Var] -> Var
-- generate a fresh variable name 
-- if a letter is used, go to the next letter (a->b, b->c, etc.)
-- if z is used, go to a', b', c', etc.
-- if z' is used, go to a'', b'', c'', etc.
-- continue this pattern

fresh xs = head [x | x <- map (:[]) ['a'..'z'] ++ map ((++"'") . (:[])) ['a'..'z'], x `notElem` xs]


-- e[d/x] = (d,x) e
subst :: (LExp,Var) -> LExp -> LExp
subst (d, x) (V y) = if x == y then d else V y
subst (d, x) (A t1 t2) = A (subst (d, x) t1) (subst (d, x) t2)
subst (d, x) (L y t1)
  | x == y = L y t1
  | y `elem` free d = let z = fresh (free d) in
                      L z (subst (d, x) (swapname (y, z) t1))
  | otherwise = L y (subst (d, x) t1)

-- tests for subst
-- if the bound variable is free in the substitution, then it is renamed
test1 = subst (V "x", "y") (V "y") == V "x"
-- if the bound variable is not free in the substitution, then it is not renamed
test2 = subst (V "x", "y") (V "z") == V "z"
-- if we have an application, then we substitute in both subexpressions
test3 = subst (V "x", "y") (A (V "y") (V "z")) == A (V "x") (V "z")
-- in the lambda we have to be careful to avoid capture
-- case 1. the substitution is the same as the bound variable, then we don't rename
test4 = subst (V "x", "y") (L "y" (V "y")) == L "y" (V "y")
-- case 2. the substitution is different from the bound variable, but the bound variable is not free in the substitution. Then we substitute in the body
test5 = subst (L "a" (V "a") , "x") (A (L "y" (V "x")) (V "y")) == A (L "y" (L "a" (V "a"))) (V "y")
-- case 3. the substitution is different from the bound variable, and the bound variable is free in the substitution. Then we substitute and rename.
test6 = subst (A (L "a" (V "y")) (V "a"), "x") (A (L "y" (V "x")) (V "y")) == A (L "b" (A (L "a" (V "y")) (V "a"))) (V "y")

