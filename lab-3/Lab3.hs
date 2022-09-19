import Data.List

--- Zipping exercises

-- Exercise 1a
my_zip :: [a] -> [b] -> [(a,b)]
my_zip la lb = zipWith (\x y -> (x,y)) la lb

-- Exercise 1b
my_zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
my_zipWith f la lb = map (\(x,y) -> f x y) (zip la lb)

-- Exercise 1c (optional)
my_transpose :: [[a]] -> [[a]]
my_transpose [] = []
my_transpose [x] = map (\elem -> [elem]) x
my_transpose (x:xs) = zipWith (\elem col -> elem:col) x (my_transpose xs)

--- Folding exercises

-- Exercise 2a
altsum :: Num a => [a] -> a
altsum xs = foldr (\x y -> x-y) 0 xs

-- Exercise 2b
my_intersperse :: a -> [a] -> [a]
my_intersperse x xs = foldr (my_put x) [] xs

my_put :: a -> a -> [a] -> [a]
my_put _ u [] = [u]
my_put x u us = u:x:us

-- Exercise 2c
my_tails :: [a] -> [[a]]
my_tails xs = foldr (make_tails) [] xs

make_tails :: a -> [[a]] -> [[a]]
make_tails x [] = [[x], []]
make_tails x (u:us) = (x:u):(u:us)

-- Exercise 2d (optional)
my_isPrefixOf :: Eq a => [a] -> [a] -> Bool
-- my_isPrefixOf s1 s2 = foldr () True s1

-- using zip
my_isPrefixOf s1 s2 = if (length s1) > (length s2) 
                        then False
                        else foldr (\(x1,x2) b -> (x1 == x2) && b) True (zip s1 s2)

-- Exercise 2e (optional)
my_dropWhile :: (a -> Bool) -> [a] -> [a]
my_dropWhile = undefined

{- my_takeWhile f xs = foldr (\u us -> if f u then u:us else []) [] xs -}

--- Difference lists

type DiffList a = [a] -> [a]

toDL :: [a] -> DiffList a
toDL xs = (xs++)

fromDL :: DiffList a -> [a]
fromDL dxs = dxs []

cons :: a -> DiffList a -> DiffList a
cons x dxs = (x:) . dxs

snoc :: DiffList a -> a -> DiffList a
snoc dxs x = dxs . (x:)

-- Exercise 3a
toDLrev :: [a] -> DiffList a
toDLrev [] = toDL []
toDLrev (x:xs) = snoc (toDLrev xs) x

-- Exercise 3b
my_reverse :: [a] -> [a]
my_reverse xs = (fromDL . toDLrev) xs

naive_reverse :: [a] -> [a]
naive_reverse []     = []
naive_reverse (x:xs) = naive_reverse xs ++ [x]

--- Regular expression matching

data RegExp = Zero | One
            | C Char
            | Plus RegExp RegExp | Times RegExp RegExp
            | Star RegExp
  deriving (Show,Eq)

accept :: RegExp -> String -> Bool

accept e w = acc e w null

-- Exercise 4a
acc :: RegExp -> String -> (String -> Bool) -> Bool
acc Zero          w k = False
acc One           w k = k w
acc (C a)         w k = case w of 
                          "" -> False
                          (x:xs) -> (x == a) && (k xs)

acc (Plus e1 e2)  w k = (acc e1 w k) || (acc e2 w k)
acc (Times e1 e2) w k = acc e1 w (\suffix_w -> acc e2 suffix_w k)

-- Exercise 4b (optional)
acc (Star e)      w k = (acc One w k) || (acc e w k) || (acc (Times e Star e) w k)
                          


