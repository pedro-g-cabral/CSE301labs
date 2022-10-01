--- Programming in untyped lambda calculus

-- Encodings of booleans and natural numbers from class
{-
true = \x.\y.x
false = \x.\y.y
not = \b.\x.\y.b y x
and = \b.\c.b c false
zero = \f.\x.x
one = \f.\x.f x
two = \f.\x.f (f x)
succ = \n.\f.\x.f (n f x)
add = \m.\n.m succ n
mult = \m.\n.m (add n) 0
isZero = \n.n (\b.false) true
-}

-- Exercise 1a
{-
isEven = \n.n not true
-}

-- Exercise 1b
{- 
exp = \m.\n.n (mult m) one
-}

-- Encodings of pairing and projections
{-
pair = \x.\y.\f.f x y
fst = \p.p (\x.\y.x)
snd = \p.p (\x.\y.y)
-}

-- Exercise 1c
{-
swap = \p.\f.f (snd p) (fst p)
-}

-- Exercise 1d
{-
swapIf \b.\p.b (swap p) p
-} 

-- Exercise 1e (optional)
{-
fib_pair = \p.\(pair (snd p) (add (fst p) (snd p)))
fib = \n.fst (n fib_pair (pair zero one))
-}

-- Exercise 1e (optional)
{-
pred = <your definition here>
-}

-- Curry's and Turing's fixed point operators
{-
Y = \x.(\y.x(y y))(\y.x (y y))
Theta = (\x.\y.y (x x y)) (\x.\y.y (x x y))
-}

-- Exercise 1f (optional)
{-
collatz = <your definition here>
-}

--- STLC and type inference

-- Exercise 2a
{-
e1 :: (a -> b) -> a -> b
e2 :: (a -> a) -> a -> a
e3 :: ((a -> a) -> b) -> b
e4 :: (a -> a -> b) -> a -> b
e5 :: Does not have a simple type.
If type of y is A -> B, then A = A->B.

e6 :: (a -> b) -> ((a -> b) -> a) -> b
-}


-- Exercise 2b
fn1 :: a -> b -> (a -> b -> c) -> c
fn1 = \x y z -> z x y

fn2 :: a -> b -> (a -> b -> c) -> (a,b,c)
fn2 = \x y z -> (x, y, z x y)

fn3 :: ([a] -> b) -> a -> b
fn3 = \x y -> x [y]

fn4 :: ((a -> a) -> b) -> b
fn4 = \x -> x (\y -> y)

-- Exercise 2c (optional)
{-

Intuition:
fn : (Int -> Int) -> Int
fn f = f 0

For this, we need an element (0 in this case) of type Int. A similiar
intuition could be applied to the general case, but we cannot be sure
that it exists an element of type a.

mysterylam = ??
-}


-- Exercise 2d (optional)
mysteryfn = undefined

--- Bidirectional typing

data Ty = TV Int | Fn Ty Ty
    deriving (Show,Eq)

data Expr = V Int | A Expr Expr | L Int Expr | Ann Expr Ty
    deriving (Show,Eq)

bcomp = L 0 $ L 1 $ L 2 $ A (V 0) (A (V 1) (V 2))

oneid = A (Ann (L 0 $ L 1 $ A (V 0) (V 1)) (Fn (Fn (TV 0) (TV 0)) (Fn (TV 0) (TV 0)))) (L 0 $ V 0)

type TyCxt = [(Int,Ty)]

check :: TyCxt -> Expr -> Ty -> Bool
synth :: TyCxt -> Expr -> Maybe Ty

-- Exercise 3
check gamma (Ann e a) b = if (a==b) then True else False
check gamma (V x) a = ((x, a) `elem` gamma)
check gamma (L x e) (Fn a b) = check ((x,a):gamma) e b
check gamma e b = case t of
                    Nothing -> False
                    (Just a) -> (a == b)
                   where t = synth gamma e


synth gamma (Ann e a) = (Just a)
synth gamma (V x) = foldr (\(x1, a1) r -> if x1==x then (Just a1) else r) Nothing gamma
synth gamma (A e1 e2) = case t of
                            Just (Fn a b) -> if (check gamma e2 a) then (Just b) else Nothing
                            otherwise -> Nothing
                         where t = synth gamma e1