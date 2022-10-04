import Control.Monad.State
import Control.Monad.Fail
import System.Random
import Data.List

data Expr = Con Double | Sub Expr Expr | Div Expr Expr
    deriving (Show,Eq)

e1 = Sub (Div (Con 2) (Con 4)) (Con 3)
e2 = Sub (Con 1) (Div (Con 2) (Con 2))
e3 = Div (Con 1) (Sub (Con 2) (Con 2))

-- Exercise 1a
evalSafe :: Expr -> Maybe Double
evalSafe (Con x) = return x
evalSafe (Sub e1 e2) = do {
  x1 <- evalSafe e1;
  x2 <- evalSafe e2;
  return (x1-x2)
}

evalSafe (Div e1 e2) = do {
  x1 <- evalSafe e1;
  x2 <- evalSafe e2;
  if (x2==0) then Nothing else return (x1/x2)  
}

-- Exercise 1b
evalSafeMF :: MonadFail m => Expr -> m Double
evalSafeMF (Con x) = return x
evalSafeMF (Sub e1 e2) = do {
  x1 <- evalSafeMF e1;
  x2 <- evalSafeMF e2;
  return (x1 - x2)
}

evalSafeMF (Div e1 e2) = do {
  x1 <- evalSafeMF e1;
  x2 <- evalSafeMF e2;
  if (x2==0) then (fail "Division by Zero Error") else return (x1/x2)
}

{- different outputs of evalSafeMF ... 

> evalSafeMF e1
-2.5

> evalSafeMF e2
0.0

> evalSafeMF e3
Exception: user error (Division by Zero Error)

-}

evalWeird :: Expr -> StateT Int Maybe Double
evalWeird (Con c)    =
  get >>= \n ->
  put (n+1) >>= \_ ->
  return (if n `mod` 3 == 2 then 0 else c)
evalWeird (Sub e1 e2) =
  evalWeird e1 >>= \x1 ->
  evalWeird e2 >>= \x2 ->
  return (x1-x2)
evalWeird (Div e1 e2) =
  evalWeird e1 >>= \x1 ->
  evalWeird e2 >>= \x2 ->
  if x2 /= 0 then return (x1/x2) else lift Nothing

evalWeirdTop e = runStateT (evalWeird e) 0 >>= \(x,s) -> return x

-- Exercise 1c
{-
Alternative solution: Change the order!
x2 <- evalWeird' e2
x1 <- evalWeird' e1
-}
evalWeird' :: MonadFail m => Expr -> StateT Int m Double
evalWeird' (Con x) =
  get >>= \n -> 
  put (n-1) >>= \_ ->
  return (if (n `actualMod` 3 == 0) then 0 else x)
evalWeird' (Sub e1 e2) = do {
  x1 <- evalWeird' e1;
  x2 <- evalWeird' e2;
  return (x1-x2)
}
evalWeird' (Div e1 e2) = do {
  x1 <- evalWeird' e1;
  x2 <- evalWeird' e2;
  if (x2==0) then (fail "Division by Zero Error") else return (x1/x2)
}

evalWeirdTop' :: MonadFail m => Expr -> m Double
evalWeirdTop' e = runStateT (evalWeird' e) (exprSize e) >>= \(x,s) -> return x

{- Number of constants in expression -}
exprSize :: Expr -> Int
exprSize (Con x) = 1
exprSize (Sub e1 e2) = (exprSize e1) + (exprSize e2)
exprSize (Div e1 e2) = (exprSize e1) + (exprSize e2)

{- a mod b, but if a is negative the result is still in [0, b-1] -}
actualMod :: Int -> Int -> Int
actualMod a b = (b + (a `mod` b)) `mod` b

data Bin a = L a | B (Bin a) (Bin a)
  deriving (Show,Eq)

mapBin :: (a -> b) -> Bin a -> Bin b
mapBin f (L x)     = L (f x)
mapBin f (B tL tR) = B (mapBin f tL) (mapBin f tR)

instance Functor Bin where
  fmap = mapBin

-- Exercise 2a
{- 
We can prove both results by induction on the depth of the binary tree. The key idea
is that when solving the induction step (B tL tR), we can apply the induction hypothesis
to tL and tR.

Base case:
  mapBin id (L x) = L (id x) = L x 
  mapBin (f . g) (L x) = L (f (g x)) = mapBin f (L (g x)) = mapBin f (mapBin g (L x))

Induction Step:
  mapBin id (B tL tR) = B (mapBin id tL) (mapBin id tR) = B tL tR 
  mapBin (f . g) (B tL tR) = B (mapBin (f . g) tL) (mapBin (f . g) tR) 
    = B (mapBin f (mapBin g tL)) (mapBin f (mapBin g tR)) 
    = mapBin f (B (mapBin g tL) (mapBin g tR))
    = mapBin f (mapBin g (B tL tR))

As desired.

 -}

-- Exercise 2b
instance Monad Bin where
  return x = L x
  (>>=) (L x) f = f x
  (>>=) (B tL tR) f = B (tL >>= f) (tR >>= f)

instance Applicative Bin where
  pure = return
  xm <*> ym = xm >>= \x -> ym >>= return . x

-- Exercise 2c (optional)
{- Your proof goes here 

return x >>= f = >>= (L x) f = f x

Proof of t >>= return by induction

  Base case:
    (L x) >>= return = (L x) >>= return = return x = L x

  Induction step:
    (B tL tR) >>= return = B (tL >>= return) (tR >>= return)
      = B (tL) (tR) = t

We also prove (t >>= f) >>= g = t >>= (\x -> (f x >>= g)) by induction 

((L x) >>= f) >>= g = (f x) >>= g
((B tL tR) >>= f) >>= g = (B (tL >>= f) (tR >>= f)) >>= g
  = B ((tL >>= f) >>= g) ((tR >>= f) >>= g) 
  = B (tL >>= (\x -> (f x >>= g))) (tR >>= (\x -> (f x >>= g)))
  = (B tL tR) >>= (\x -> (f x >>= g))

As desired.

-}

-- Exercise 2d (optional)
{- Your thoughts go here -}

class Monad m => SelectMonad m where
  select :: [a] -> m a

instance SelectMonad [] where
  select = id

instance SelectMonad IO where
  select xs
    | not (null xs) = do i <- getStdRandom (randomR (0, length xs-1))
                         return (xs !! i)
    | otherwise     = fail "cannot select from empty list"

newtype Dist a = Dist { dist :: [(a,Rational)] }  deriving (Show)

instance Monad Dist where
  return x = Dist [(x,1)]
  xm >>= f = Dist [(y,p*q) | (x,p) <- dist xm, (y,q) <- dist (f x)]
  
-- We add the following standard boilerplate to derive instances of the
-- Functor and Applicative type classes, from the Monad instance above:
instance Functor Dist where
  fmap f xm = xm >>= return . f

instance Applicative Dist where
  pure = return
  xm <*> ym = xm >>= \x -> ym >>= return . x

instance SelectMonad Dist where
  select xs
    | not (null xs) = let n = length xs in Dist [(x, 1 / fromIntegral n) | x <- xs]
    | otherwise     = error "cannot select from empty list"

code :: SelectMonad m => m Char
code = do
  i <- select [0..3]
  return ("hello" !! i)

prob :: Eq a => Dist a -> a -> Rational
prob xm x = sum [p | (y,p) <- dist xm, x == y]

normalize :: Eq a => Dist a -> Dist a
normalize xm = Dist [(x,prob xm x) | x <- support xm]
  where
    support :: Eq a => Dist a -> [a]
    support xm = nub [x | (x,p) <- dist xm, p > 0]  -- "nub", defined in Data.List, removes duplicates

-- Exercise 3a
coin :: SelectMonad m => m Bool
coin = do{
  x <- select [0,1];
  if x == 1 then return True else return False
}
  
-- Exercise 3b
subset :: SelectMonad m => [a] -> m [a]
subset [] = return []
subset (x:xs) = do{
  c <- coin;
  res <- subset xs;
  if c then return x++res else return res
}

-- Exercise 3c
simulate :: Monad m => Int -> m Bool -> m Int
simulate = undefined

-- Exercise 3d (optional)
genTree :: SelectMonad m => [a] -> m (Bin a)
genTree = undefined

