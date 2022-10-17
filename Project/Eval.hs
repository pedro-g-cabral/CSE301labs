module Eval (normalize) where

import Expr
import Subst

-- datatype of one-hole contexts for lambda expressions
data LExpCxt = Hole | A'1 LExpCxt LExp | A'2 LExp LExpCxt | L' Var LExpCxt
  deriving (Show,Eq)

-- we represent contexts "inside-out", i.e., with the parts of the
-- context nearest to the hole at the top-level.
-- The plugging function is defined accordingly.
plug :: LExpCxt -> LExp -> LExp
plug Hole       d = d
plug (A'1 c e2) d = plug c (A d e2)
plug (A'2 e1 c) d = plug c (A e1 d)
plug (L' x c)   d = plug c (L x d)

-- a pointer to a subexpression (a.k.a. "zipper") is a pair of a context and an expression
type LExpPtr = (LExpCxt,LExp)

-- a template for implementing normalize, as described in the
-- mini-project page...

subexp :: LExp -> [LExpPtr]
subexp = undefined

redex :: LExp -> [LExpPtr]
redex = undefined

stepBeta :: LExp -> [LExp]
stepBeta = undefined

normalize :: LExp -> LExp
normalize = undefined


