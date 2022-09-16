pattern-matching shapes of possible inputs
higher-order functions = functions as inputs and as outputs
rigorous typing = verifiable interfaces for functions

- Write concisely
- Easier to parallelize
- Better control over "side-effects" of functions

Alonzo Church + Kleene + Rosser = lambda-calculus
Turing: TM-computablity == lambda-definability

domain theory
polymorphic lambda-calculus
polymorphic type inference algorithm
dependent type theory
monads for FP to describe side-effects

Haskell is pure & lazy

- purity is about side-effects
- laziness (expensive for the compiler and the programmer)

if/else statements always return a value, so else is mandatory

ranges
[1..20]
[2,4..20]
[3,6..20]

from 20 to 1, do [20,19..1]

list comprehension
[x*2 | x <- [1..10]]
[x*2 | x <- [1..10], x*2 >= 12]
[ x | x <- [10..20], x /= 13, x /= 15, x /= 19]

When drawing from several lists, comprehensions produce all combinations of the given lists and then join them by the output function we supply. 

ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]  
[16,20,22,40,50,55,80,100,110]

ghci> [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]  
[55,80,100,110]  

Actually, in Haskell, String is just a synonym for [Char], the type of lists of characters. This means in particular that any generic operation on lists (such as concatenation ++) can also be applied to strings.

Rational: the type of arbitrary-precision fractions.

You can cast

Integer = arbitrary precision integer
Int = integer in [-2^29, 2^29-1]

> x :: Int


casts an Integer to an Int 

The components of a pair can be extracted using the functions fst and snd, which are examples of polymorphic functions in the sense that they work irrespective of the types of the components.

[] == empty list which is pronounced 'nil'

> mod x y 


works as expected

> list !! index 

for list[index]

The operations take n xs and drop n xs respectively take and drop the first n elements of a list xs.

null list

determines whether a list is empty

Pattern-matching is a concise notation for defining functions over data types, which works by partitioning the range of possible inputs into a finite set of mutually-exclusive cases. It is especially powerful when combined with user-defined data types, but it is also very convenient as a way of defining functions over lists. 

null []     = True
null (x:xs) = False

null xs = case xs of
            []    -> True
            (_:_) -> False

take n (x:xs)
    | n == 0    = []
    | n > 0     = x : take (n-1) xs
    | otherwise = error "n negative"

