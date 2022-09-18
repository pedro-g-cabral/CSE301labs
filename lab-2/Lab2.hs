--- Proving exercises (from Lecture 1 notes)

-- Exercise 3.1
{-

(1) We know that 
      [] ++ xs == xs 
by definition.

(2) We prove by structural induction that
      xs ++ [] == xs

First, we know by (1) that
      [] ++ [] == []
Next, assume that (2) holds for a list xs. We prove that it holds for x:xs.
      x:xs ++ [] == x:(xs++[]) -- (by definition)
      x:(xs++[]) == x:(xs) -- (by induction hyptothesis)
Therefore (2) also holds for x:xs. We are done by structural induction.

(3) We also prove by structural induction that
      xs++(ys++zs) == (xs++ys)++zs
for all lists xs, ys, zs. We prove it by structural induction on xs. First,
      []++(ys++zs) == ys++zs -- (by (1))
      ([]++ys)++zs == (ys)++zs == ys ++ zs (also by (1))
Now assume that the property holds for the list xs. We prove that it holds
for the list x:xs. We have:
      (x:xs)++(ys++zs) == x:(xs++(ys++zs)) -- (by definition of ++)
      == x:((xs++ys)++zs) -- (by induction hypthesis)
      == (x:(xs++ys))++zs -- (by definition of ++)
      ==((x:xs)++ys) ++ zs -- (by definition of ++)
The proof is complete by structural induction.

-}

-- Exercise 3.2 (optional)

-- Exercise 3.3 (optional)

-- Exercise 3.4 (optional)

--- Programming exercises

import Data.Maybe

-- Exercise 0a
doubleList :: [a] -> [a]
doubleList [] = []
doubleList (x:xs) = (x:(x:(doubleList xs)))

-- Exercise 0b
firstDoubled :: Eq a => [a] -> Maybe a
firstDoubled [] = Nothing
firstDoubled [x] = Nothing
firstDoubled (x0:(x1:xs)) = if x0 == x1
                              then Just x0
                              else firstDoubled (x1:xs)

data Allergen = Nuts | Gluten | Soy | Dairy      deriving (Show, Eq)

type Recipe   = [Allergen]

type Name     = String
type Price    = Int
data Cupcake  = CC Name Recipe Price             deriving (Show,Eq)

r1, r2, r3, r4, r5 :: Recipe
r1 = [Gluten]
r2 = []
r3 = [Nuts]
r4 = [Dairy,Gluten]
r5 = [Soy]

onsale :: [Cupcake]
onsale = [CC "Chocolate Surprise" r1 200,
          CC "Lemon Mayhem" r2 150,
          CC "Peanut Butter Bliss" r3 150,
          CC "Yogurt Truly" r4 250,
          CC "Caramel Karma" r5 200]


getPrice :: Cupcake -> Price
getPrice (CC _ _ price) = price

getName :: Cupcake -> Name
getName (CC name _ _) = name

getAllergens :: Cupcake -> [Allergen]
getAllergens (CC _ recipe _) = recipe

-- Exercise 1a
priceRange :: Price -> Price -> [Cupcake] -> [Name]
priceRange _ _ [] = []
priceRange minPrice maxPrice (c: cs) = if getPrice c >= minPrice
                                       then if getPrice c <= maxPrice
                                             then (getName c : (priceRange minPrice maxPrice cs))
                                             else (priceRange minPrice maxPrice cs)
                                       else (priceRange minPrice maxPrice cs)

-- Exercise 1b
allergyFree :: [Allergen] -> [Cupcake] -> [Name]
allergyFree [] cs = mapCupcakeToName cs
allergyFree (a : as) cs = allergyFree as (mAllergyFree a cs) 

mapCupcakeToName :: [Cupcake] -> [Name]
mapCupcakeToName [] = []
mapCupcakeToName (c:cs) = (getName c : mapCupcakeToName cs)

mAllergyFree :: Allergen -> [Cupcake] -> [Cupcake]
mAllergyFree _ [] = []
mAllergyFree a (c:cs) = if containsAllergen c a
                           then mAllergyFree a cs
                           else (c : mAllergyFree a cs)

containsAllergen :: Cupcake -> Allergen -> Bool
containsAllergen c a = isInList a (getAllergens c)

isInList :: Eq a => a -> [a] -> Bool
isInList _ [] = False
isInList x (y:ys) = if x == y
                     then True
                     else isInList x ys


------- Exercise 2 -------

type Tin = [Recipe]
data Spec = And Spec Spec | Or Spec Spec | Not Spec | HasCup Int Allergen  deriving (Show,Eq)

sampletin :: Tin
sampletin = [r3,r4,r2,r5]

-- Exercise 2a
checkSpec :: Spec -> Tin -> Bool
checkSpec (HasCup 0 x) (t:ts) = (isInList x t)
checkSpec (HasCup k x) (t:ts) = checkSpec (HasCup (k-1) x) ts
checkSpec (And s1 s2) t = (checkSpec s1 t) && (checkSpec s2 t)
checkSpec (Or s1 s2) t = (checkSpec s1 t) || (checkSpec s2 t)
checkSpec (Not s) t = not (checkSpec s t)

-- Exercise 2b (optional)
checkSpec' :: Spec -> Tin -> Maybe Bool
checkSpec' (HasCup n x) [] = Nothing
checkSpec' (HasCup 0 x) (t:ts) = Just (isInList x t)
checkSpec' (HasCup k x) (t:ts) = checkSpec' (HasCup (k-1) x) ts
checkSpec' (And s1 s2) t = mAnd (checkSpec' s1 t) (checkSpec' s2 t)
checkSpec' (Or s1 s2) t = mOr (checkSpec' s1 t) (checkSpec' s2 t)
checkSpec' (Not s) t = mNor (checkSpec' s t)

mAnd :: Maybe Bool -> Maybe Bool -> Maybe Bool
mAnd Nothing _ = Nothing
mAnd _ Nothing = Nothing
mAnd (Just a) (Just b) = Just (a && b)

mOr :: Maybe Bool -> Maybe Bool -> Maybe Bool
mOr Nothing _ = Nothing
mOr _ Nothing = Nothing
mOr (Just a) (Just b) = Just (a || b)

mNor :: Maybe Bool -> Maybe Bool
mNor Nothing = Nothing
mNor (Just a) = Just (not a)


------- Exercise 3 -------

data Tree a b = Leaf a | Node b [Tree a b]  deriving (Show,Eq)

texample :: Tree Char Integer
texample = Node 1 [Node 2 [Leaf 'a', Leaf 'b'], Node 3 [Leaf 'c', Leaf 'd', Leaf 'e'], Node 4 []]

bst :: Tree () Char
bst = Node 'c' [Node 'a' [Leaf (), Node 'b' [Leaf (), Leaf ()]], Node 'd' [Leaf (), Leaf ()]]

-- Exercise 3a
canopy :: Tree a b -> [a]
canopy (Leaf l) = [l]
canopy (Node _ []) = []
canopy (Node _ (x:xs)) = (canopy x) ++ (concat (map canopy xs))

-- Exercise 3b (optional)
preorder :: Tree a b -> [Either a b]
preorder (Leaf x) = [Left x]
preorder (Node x []) = [Right x]
preorder (Node x children) = (Right x:(concat (map preorder children)))

-- Exercise 4
linearSort :: Ord a => [a] -> [a]
linearSort xs = linearSortAux xs []

linearSortAux :: Ord a => [a] -> [a] -> [a]
linearSortAux [] stack = stack
linearSortAux (x:xs) [] = linearSortAux xs [x]
linearSortAux (x:xs) (s:stackTail) = if x > s
                                       then (s : linearSortAux (x:xs) stackTail)
                                       else linearSortAux xs (x: (s : stackTail))


------- Exercise 5 -------

-- Exercise 5a (optional)
counterexample :: [Int]
counterexample = [2,3,1]

data Bin = L | B Bin Bin  deriving (Show,Eq)

-- Exercise 5b (optional)
{-

Desired behavior:

If we have a binary tree (B left right), we wish to output the permutation:

(fromBin left) ++ [size (B left right)] ++ [size left + x | x <- (fromBin right)]


-}
fromBin :: Bin -> [Int]
fromBin L = []
fromBin (B left right) = (fromBin left) ++ [sizeTree] ++ [sizeL + x | x <- (fromBin right)]
                        where
                              (sizeL, _, sizeTree) = getSizes (B left right)

-- Returns sizes of left, right, tree
getSizes :: Bin -> (Int, Int, Int)
getSizes L = (0, 0, 0)
getSizes (B left right) = (sizeL, sizeR, 1+sizeL+sizeR)
                        where
                              (_, _, sizeL) = getSizes left
                              (_, _, sizeR) = getSizes right

{-
Desired behavior:

First finds n (the maximum number of the list). Make n the root of the tree.

What comes before n in the list, will be used to generate the left subtree. The
same is done for the right subtree.

-}
toBin :: [Int] -> Maybe Bin
toBin [] = Just L
toBin xs = toBinAux (length xs) xs

-- (length, list) -> Binary tree
toBinAux :: Int -> [Int] -> Maybe Bin
toBinAux 0 [] = Just L
toBinAux 1 [1] = Just (B L L)
toBinAux n xs = case searchResult of
                        Nothing -> Nothing
                        (Just p) -> makeMaybeBin (toBinAux p leftList) (toBinAux (n-p-1) rightList)
                              where
                                    leftList = take p xs
                                    rightList = [x-p | x <- drop (p+1) xs]
                  where
                        searchResult = findInList 0 n xs

makeMaybeBin :: Maybe Bin -> Maybe Bin -> Maybe Bin
makeMaybeBin Nothing _ = Nothing
makeMaybeBin _ Nothing = Nothing
makeMaybeBin (Just left) (Just right) = Just (B left right)

findInList :: Eq a => Int -> a -> [a] -> Maybe Int
findInList _ _ [] = Nothing
findInList n x0 (x:xs) = if x0 == x
                              then (Just n)
                              else findInList (n+1) x0 xs