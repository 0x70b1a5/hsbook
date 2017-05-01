{-
		8.6 Chapter Exercises

		Review of types

		1. What is the type of [[True, False], [True, True], [False,
		True]]?
		a) Bool
		b) mostly True
		c) [a]
		d) [[Bool]]

D


		2. Which of the following has the same type as [[True, False],
		[True, True], [False, True]]?
		a) [(True, False), (True, True), (False, True)]
		b) [[3 == 3], [6 > 5], [3 < 4]]
		c) [3 == 3, 6 > 5, 3 < 4]
		d) ["Bool", "more Bool", "Booly Bool!"]

B

		3. For the following function
		func :: [a] -> [a] -> [a]
		func x y = x ++ y
		which of the following is true?
		a) x and y must be of the same type
		b) x and y must both be lists
		c) if x is a String then y must be a String
		d) all of the above

D

		4. For the func code above, which is a valid application of
		func to both of its arguments?
		a) func "Hello World"
		b) func "Hello" "World"
		c) func [1, 2, 3] "a, b, c"
		d) func ["Hello", "World"]

D

		Reviewing currying

		Given the following definitions, tell us what value results from
		further applications.
		cattyConny :: String -> String -> String
		cattyConny x y = x ++ " mrow " ++ y
		-- fill in the types
		flippy = flip cattyConny

flippy :: String -> String -> String

		appedCatty = cattyConny "woops"

appedCatty = String -> String

		frappe = flippy "haha"

frappe = String -> String

		1. What is the value of appedCatty "woohoo!" ? Try to determine
		the answer for yourself, then test in the REPL.

"woops mrow woohoo!"

		2. frappe "1"

"1 mrow haha"

		3. frappe (appedCatty "2")

"haha mrow woops mrow 2"

		4. appedCatty (frappe "blue")

"woops mrow blue mrow haha"

		5. cattyConny (frappe "pink")
		(cattyConny "green" (appedCatty "blue"))

you have to be kidding me
pink mrow haha mrow green mrow woops mrow blue

		6. cattyConny (flippy "Pugs" "are") "awesome"

are mrow Pugs mrow awesome

		Recursion

		1. Write out the steps for reducing dividedBy 15 2 to its final
		answer according to the Haskell code.

...no

		2. Write a function that recursively sums all numbers from
		1 to n, n being the argument. So that if n was 5, you’d add
		1 + 2 + 3 + 4 + 5 to get 15. The type should be (Eq a, Num a)
		=> a -> a.

recSum 0 = 1
recSum n = n + recSum n-1

		3. Write a function that multiplies two integral numbers
		using recursive summation. The type should be (Integral
		a) => a -> a -> a.

recMul n 0 = 0
recMul n m = n + recMul n (abs(m)-1)
  
		Fixing dividedBy
		Our dividedBy function wasn’t quite ideal. For one thing. It
		was a partial function and doesn’t return a result (bottom)
		when given a divisor that is 0 or less.
		Using the pre-existing div function we can see how negative
		numbers should be handled:
		Prelude> div 10 2
		5
		Prelude> div 10 (-2)
		-5
		Prelude> div (-10) (-2)
		5
		Prelude> div (-10) (2)
		-5
		The next issue is how to handle zero. Zero is undefined
		for division in math, so really we ought to use a datatype that
		lets us say there was no sensible result when the user divides
		by zero. If you need inspiration, consider using the following
		datatype to handle this.
		data DividedResult =
		Result Integer
		| DividedByZero

data DivResult = Result (Integer, Integer) | DivByZero deriving Show

dividedBy :: Integer -> Integer -> DivResult
dividedBy num denom = go num denom 0 False
  where go n d count neg
         | d == 0 = DivByZero
         | and [abs n < abs d, neg] = Result ((-count), n)
         | and [abs n < abs d, not neg] = Result (count, n)
         | n < 0 = go ((-n) - d) d (count + 1) True
         | d < 0 = go (n - (-d)) (-d) (count + 1) True
         | otherwise = go (n - d) d (count + 1) neg

		McCarthy 91 function
		We’re going to describe a function in English, then in math
		notation, then show you what your function should return for
		some test inputs. Your task is to write the function in Haskell.
		The McCarthy 91 function yields 𝑥 − 10 when 𝑥 > 100 and 91
		otherwise. The function is recursive.
		mc91 = undefined
		You haven’t seen map yet, but all you need to know right
		now is that it applies a function to each member of a list and
		returns the resulting list. It’ll be explained in more detail in
		the next chapter.
		Prelude> map mc91 [95..110]
		[91,91,91,91,91,91,91,92,93,94,95,96,97,98,99,100]

m n | n > 100 = n - 10 | otherwise = m $ m $ n + 11

		Numbers into words
		module WordNumber where
		import Data.List (intersperse)
		digitToWord :: Int -> String
		digitToWord n = undefined
		digits :: Int -> [Int]
		digits n = undefined
		wordNumber :: Int -> String
		wordNumber n = undefined
		Here undefined is a placeholder to show you where you need
		to fill in the functions. The n to the right of the function names
		is the argument which will be an integer.
		Fill in the implementations of the functions above so that
		wordNumber returns the English word version of the Int value.
		You will first write a function that turns integers from 0-9 into
		their corresponding English words, ”one,” ”two,” and so on.
		Then you will write a function that takes the integer, separates
		the digits, and returns it as a list of integers. Finally you will
		need to apply the first function to the list produced by the second
		function and turn it into a single string with interspersed
		hyphens.
		We’ve laid out multiple functions for you to consider as you
		tackle the problem. You may not need all of them, depending
		on how you solve it–these are just suggestions. Play with
		them and look up their documentation to understand them
		in deeper detail.
		You will probably find this difficult.
		div :: Integral a => a -> a -> a
		mod :: Integral a => a -> a -> a
		map :: (a -> b) -> [a] -> [b]
		concat :: [[a]] -> [a]
		intersperse :: a -> [a] -> [a]
		(++) :: [a] -> [a] -> [a]
		(:[]) :: a -> [a]
		Also consider:
		Prelude> div 135 10
		13
		Prelude> mod 135 10
		5
		Prelude> div 13 10
		1
		Prelude> mod 13 10
		3
		Here is what your output should look in the REPL when it’s
		working:
		Prelude> wordNumber 12324546
		"one-two-three-two-four-five-four-six"
		Prelude>
-}
module WordNumber where
import Data.List (intersperse)

digToWord :: Int -> String
digToWord n
        | n == 0 = "zero"
        | n == 1 = "one"
        | n == 2 = "two"
        | n == 3 = "three"
        | n == 4 = "four"
        | n == 5 = "five"
        | n == 6 = "six"
        | n == 7 = "seven"
        | n == 8 = "eight"
        | n == 9 = "nine"
        | otherwise = ""

digits :: Int -> [Int]
digits a = go a []
  where go a arr
         | div a 10 > 10 = go (div a 10) ((mod a 10):arr)
         | div a 10 == 0 = a:arr
         | otherwise = (div a 10):((mod a 10):arr)

wordNumber :: Int -> String
wordNumber = concat . (intersperse "-") . (map digToWord) . digits

