{-
1. Which (two or more) of the following are equivalent?
a) mTh x y z = x * y * z
b) mTh x y = \z -> x * y * z
c) mTh x = \y -> \z -> x * y * z
d) mTh = \x -> \y -> \z -> x * y * z

 A: _ALL_


The type of mTh (above) is Num a => a -> a -> a -> a.
Which is the type of mTh 3?
a) Integer -> Integer -> Integer
b) Num a => a -> a -> a -> a
c) Num a => a -> a
d) Num a => a -> a -> a

 A: D.


a) Rewrite the f function in the where clause.
addOneIfOdd n = case odd n of
True -> f n
False -> n
where f n = n + 1

 A: where f = \n -> n + 1


b) Rewrite the following to use anonymous lambda syntax:
addFive x y = (if x > y then y else x) + 5

 A: addFive = \x -> \y -> (if ...)

c) Rewrite the following so that it doesn’t use anonymous
lambda syntax:
mflip f = \x -> \y -> f y x

 A: mflip f x y = f y x



`newtype`
-}
module RegisteredUser where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum))
          = putStrLn $ name ++ " " ++ show acctNum

{-
EXES 7.@
Giben the following declarations
  k (x, y) = x
  k1 = k ((4-1), 10)
  k2 = k ("three", (1 + 2))
  k3 = k (3, True)
  a) What is the type of k?
  b) What is the type of k2? Is it the same type as k1 or k3?
  c) Of k1, k2, k3, which will return the number 3 as the
     result?

A) k :: (a,b) -> a
B) k2 :: String. NO
C) k1 & k3


2. Fill in the definition of the following function:
  -- Remember: Tuples have the same syntax for their
  -- type constructors and their data constructors.
  f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
  f = undefined

f (a, b, c) (d, e, f) = ((a, d), (c, f))


1. The following should return x when x is greater than y.
  functionC x y = if (x > y) then x else y

functionC' x y = 
  case compare x y of
    GT -> x
    _ -> y


2. The following will add 2 to even numbers and otherwise
  simply return the input value.
  ifEvenAdd2 n = if even n then (n+2) else n

iEA2' n = case mod n 2 of
  0 -> n+2
  1 -> n


3. The following compares a value, x, to zero and returns an
  indicator for whether x is a postive number or negative
  number. But what if x is 0? You may need to play with
  the compare function a bit to find what to do.
  nums x =
    case compare x 0 of
      LT -> -1
      GT -> 1
      EQ -> 0


  dodgy x y = x + y * 10
  oneIsOne = dodgy 1
  oneIsTwo = (flip dodgy) 2
1. For example, given the expression dodgy 1 0, what do you
  think will happen if we evaluate it? If you put the definitions
  in a file and load them in GHCi, you can do the
  following to see the result.
    Prelude> dodgy 1 0
    1
  Now attempt to determine what the following expressions
  reduce to. Do it in your head, verify in your REPL after
  you think you have an answer.
2. dodgy 1 1
  11? yes
3. dodgy 2 2
  22? yes
4. dodgy 1 2
  21 yes
5. dodgy 2 1
  12 yes
6. oneIsOne 1
  11 yes
7. oneIsOne 2
  21
8. oneIsTwo 1
  11
9. oneIsTwo 2
  22
10. oneIsOne 3
  31
11. oneIsTwo 3
  23


3. The following function returns
  pal xs
  | xs == reverse xs = True
  | otherwise = False
    a) xs written backwards when it’s True
    b) True when xs is a palindrome
    c) False when xs is a palindrome
    d) False when xs is reversed

B.


4. What types of arguments can pal take?

any list, reverse :: [a] -> [a]


6. The following function returns
  numbers x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1
  a) the value of its argument plus or minus 1
  b) the negation of its argument
  c) an indication of whether its argument is a positive or
    negative number or zero
  d) binary machine language

C.

7. What types of arguments can numbers take?
8. What is the type of the function numbers?

numbers :: (Num a, Ord a) => a -> Int


CHAP EXS
1. A polymorphic function
a) changes things into sheep when invoked
b) has multiple arguments
c) has a concrete type
d) may resolve to values of different types, depending
on inputs

D


2. Two functions named f and g have types Char -> String
and String -> [String] respectively. The composed function
g . f has the type
a) Char -> String
b) Char -> [String]
c) [[String]]
d) Char -> String -> [String]

B


3. A function f has the type Ord a => a -> a -> Bool and we
apply it to one numeric value. What is the type now?
a) Ord a => a -> Bool
b) Num -> Num -> Bool
c) Ord a => a -> a -> Integer
d) (Ord a, Num a) => a -> Bool

D


4. A function with the type (a -> b) -> c
a) requires values of three different types
b) is a higher-order function
c) must take a tuple as its first argument
d) has its parameters in alphabetical order

B


5. Given the following definition of f, what is the type of f
True?
f :: a -> a
f x = x
a) f True :: Bool
b) f True :: String
c) f True :: Bool -> Bool
d) f True :: a

A


Dis function returns the tens digit of an integral
argument.
tensDigit :: Integral a => a -> a
tensDigit x = d
where xLast = x `div` 10
d = xLast `mod` 10
a) First, rewrite it using divMod.

  tD' x = fst $ divMod x 10


b) Does the divMod version have the same type as the
original version?

  yes 


c) Next, let’s change it so that we’re getting the hundreds
digit instead. You could start it like this (though that
may not be the only possibility):
hunsD x = d2
where d = undefined

hD x = fst $ divMod x 100


2. Implement the function of the type a -> a -> Bool -> a
once each using a case expression and once with a guard.
foldBool :: a -> a -> Bool -> a
foldBool = error "Error: Need to implement foldBool!"
The result is semantically similar to if-then-else expressions
but syntactically quite different. Here is the pattern
matching version to get you started:
foldBool3 :: a -> a -> Bool -> a
foldBool3 x y True = x
foldBool3 x y False = y

fB3 x y b = 
  case b of 
    True -> x
    False -> y

fB3' x y b =
  | b = x
  | otherwise = y

3. Fill in the definition. Note that the first argument to our
function is also a function which can be applied to values.
Your second argument is a tuple, which can be used for
pattern matching:
g :: (a -> b) -> (a, c) -> (b, c)
g = undefined

g f (a, c) = ((f a), c)


-- pointfree read/show:
module Arith4 where
roundtrip :: (Show a, Read a) => a -> a
roundtrip = read . show

rtB :: (Show a, Read b) => a -> b
rtB (a :: b) = read (show (a :: b))
main = do 
  print $ rtB 4
  print $ rtB 4


