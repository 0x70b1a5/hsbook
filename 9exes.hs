import Data.Char
{-
safeTail :: [a] -> Maybe a
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x
-}
eft :: (Ord a, Enum a) => a -> a -> [a]
eft a b = go a b [a]
  where go a b arr
         | a > b = []
         | a == b = arr
         | otherwise = go (succ a) b (arr ++ [succ a])

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

{-
		Using takeWhile and dropWhile, write a function that takes a
		string and returns a list of strings, using spaces to separate
		the elements of the string into words, as in the following
		sample:
		*Main> myWords "all i wanna do is have some fun"
		["all","i","wanna","do","is","have","some","fun"]
-}

split :: String -> Char -> [String]
split s c = go s c []
  where go s c arr
         | length s == 0 = arr
         | (/= c) . head . take 1 $ s = 
             go (dropWhile (/= c) s) c (arr ++ [takeWhile (/= c) s])
         | otherwise = go (drop 1 s) c arr

myWords = (flip split) ' '

{-
		2. Next, write a function that takes a string and returns a list
		of strings, using newline separators to break up the string
		as in the following (your job is to fill in the undefined
		function):
		module PoemLines where
		firstSen = "Tyger Tyger, burning bright\n"
		secondSen = "In the forests of the night\n"
		thirdSen = "What immortal hand or eye\n"
		fourthSen = "Could frame thy fearful symmetry?"
		sentences = firstSen ++ secondSen
		++ thirdSen ++ fourthSen
		CHAPTER 9. THIS THING AND SOME MORE STUFF 467
		-- putStrLn sentences -- should print
		-- Tyger Tyger, burning bright
		-- In the forests of the night
		-- What immortal hand or eye
		-- Could frame thy fearful symmetry?
		-- Implement this
		myLines :: String -> [String]
		myLines = undefined
		-- What we want 'myLines sentences' to equal
		shouldEqual =
		[ "Tyger Tyger, burning bright"
		, "In the forests of the night"
		, "What immortal hand or eye"
		, "Could frame thy fearful symmetry?"
		]
		-- The main function here is a small test
		-- to ensure you've written your function
		-- correctly.
		main :: IO ()
		main =
		print $ "Are they equal? "
		++ show (myLines sentences == shouldEqual)
-}

myLines = (flip split) '\n'

{-
		3. Now let’s look at what those two functions have in common.
		Try writing a new function that parameterizes the
		character you’re breaking the string argument on and
		rewrite myWords and myLines using it.
-}

-- already done l0l

{-
		 look at the following functions, figure what you think
		the output lists will be, and then run them in your REPL to
		verify (note that you will need the mySqr list from above in
		scope to do this):

		[x | x <- mySqr, rem x 2 == 0]
          = [4, 16]

		[(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
          = no idea, maybe just mySqr since all y are < 50 ?
            aha it was my other idea, the EMPTY list

		take 5 [ (x, y) | x <- mySqr
		, y <- mySqr, x < 50, y > 50 ]
          = empty again, you can't fool me
-}

		-- Given the following:
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
		-- 1. First write an expression that will make tuples of the outputs
		-- of mySqr and myCube.
mSQ = [(x,y) | x <- mySqr, y <- myCube]
		--2. Now alter that expression so that it only uses the x and y
		--values that are less than 50.
mSQ50 = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]
		--3. Apply another function to that list comprehension to
		--determine how many tuples inhabit your output list.
lmsq = length mSQ50

{-
		Exercises: Bottom Madness
		Will it blow up?
		1. Will the following expression return a value or be ⊥?
		[x^y | x <- [1..5], y <- [2, undefined]]
n
		2. take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
y
		3. sum [1, undefined, 3]
y
		4. length [1, 2, undefined]
n
		5. length $ [1, 2, 3] ++ undefined
y
		6. take 1 $ filter even [1, 2, 3, undefined]
n
		7. take 1 $ filter even [1, 3, undefined]
y
		8. take 1 $ filter odd [1, 3, undefined]
n
		9. take 2 $ filter odd [1, 3, undefined]
n
		10. take 3 $ filter odd [1, 3, undefined]
y

		Intermission: Is it in normal form?
		For each expression below, determine whether it’s in:
		1. normal form, which implies weak head normal form;
		2. weak head normal form only; or,
		3. neither.
		Remember that an expression cannot be in normal form or
		weak head normal form if the outermost part of the expression
		isn’t a data constructor. It can’t be in normal form if any part
		of the expression is unevaluated.
		1. [1, 2, 3, 4, 5]
nf
		2. 1 : 2 : 3 : 4 : _
w
		3. enumFromTo 1 10
n
		4. length [1, 2, 3, 4, 5]
n
		5. sum (enumFromTo 1 10)
n
		6. ['a'..'m'] ++ ['n'..'z']
n
		7. (_, 'b')
w

		Exercises: More BOTTOMS
		As always, we encourage you to try figuring out the answers
		before you enter them into your REPL.
		1. Will the following expression return a value or be ⊥?
		take 1 $ map (+1) [undefined, 2, 3]
b
		2. Will the following expression return a value?
		take 1 $ map (+1) [1, undefined, 3]
val
		3. Will the following expression return a value?
		take 2 $ map (+1) [1, undefined, 3]
b
		4. What does the following mystery function do? What is its
		type? Describe it (to yourself or a loved one) in standard
		English and then test it out in the REPL to make sure you
		were correct.
		itIsMystery xs = map (\x -> elem x "aeiou") xs
creates array of Bools whose values are either True for each vowel in the input string or False for other characters
		5. What will be the result of the following functions:
		a) map (^2) [1..10]
range up to 10 of squares
		b) map minimum [[1..10], [10..20], [20..30]]
		-- n.b. `minimum` is not the same function
		-- as the `min` that we used before
[1, 10, 20]
		c) map sum [[1..5], [1..5], [1..5]]
[15, 15, 15]
		6. Back in the Functions chapter, you wrote a function called
		foldBool. That function exists in a module known as Data.Bool
		and is called bool. Write a function that does the same (or
		similar, if you wish) as the map (if-then-else) function you
		saw above but uses bool instead of the if-then-else syntax.
		Your first step should be bringing the bool function into
		scope by typing import Data.Bool at your Prelude prompt.
-}

bool :: Bool -> a -> a -> a
bool cond ift iff
   | cond = ift
   | otherwise = iff

{-
		Exercises: Filtering
		1. Given the above, how might we write a filter function that
		would give us all the multiples of 3 out of a list from 1-30?
ts = [ x | x <- [1..30], (rem x 3) == 0 ]
		2. Recalling what we learned about function composition,
		how could we compose the above function with the length
		function to tell us *how many* multiples of 3 there are
		between 1 and 30?
length ts
		3. Next we’re going to work on removing all articles (’the’, ’a’,
		and ’an’) from sentences. You want to get to something
		that works like this:
		Prelude> myFilter "the brown dog was a goof"
		["brown","dog","was","goof"]
		You may recall that earlier in this chapter we asked you
		to write a function that separates a string into a list of
		strings by separating them at spaces. That is a standard
		library function called words. You may consider starting
		this exercise by using words (or your version, of course).
-}
noArticles :: String -> [String]
noArticles = filter (\x -> not . elem x $ ["the", "a", "an"]) . words

{-
		Zipping exercises
		1. Write your own version of zip :: [a] -> [b] -> [(a, b)]
		and ensure it behaves the same as the original.
-}
zippo :: [a] -> [b] -> [(a, b)]
zippo [] _ = []
zippo _ [] = []
zippo (x:xs) (y:ys) = (x,y):(zippo xs ys)
{-
		2. Do what you did for zip, but now for zipWith :: (a -> b
		-> c) -> [a] -> [b] -> [c]
-}
zippoWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zippoWith f _ [] = []
zippoWith f [] _ = []
zippoWith f (x:xs) (y:ys) = (f x y):(zippoWith f xs ys)
{-
		3. Rewrite your zip in terms of the zipWith you wrote.
-}
zippoo = zippoWith (\x -> \y -> (x, y))

{-
		Chapter Exercises
		The first set of exercises here will mostly be review but will
		also introduce you to some new things. The second set is
		more conceptually challenging but does not use any syntax or
		concepts we haven’t already studied. If you get stuck, it may
		help to flip back to a relevant section and review.
		Data.Char
		These first few exercises are straightforward but will introduce
		you to some new library functions and review some of what
		we’ve learned so far. Some of the functions we will use here
		are not standard in Prelude and so have to be imported from
		a module called Data.Char. You may do so in a source file (recommended)
		or at the Prelude prompt with the same phrase:
		import Data.Char (write that at the top of your source file). This
		brings into scope a bunch of new standard functions we can
		play with that operate on Char and String types.
		1. Query the types of isUpper and toUpper.
		2. Given the following behaviors, which would we use to
		write a function that filters all the uppercase letters out
		of a String? Write that function such that, given the input
		“HbEfLrLxO,” your function will return “HELLO.”
		Prelude Data.Char> isUpper 'J'
		True
		Prelude Data.Char> toUpper 'j'
		'J'
-}
uppersFrom = filter isUpper
{-
		3. Write a function that will capitalize the first letter of a
		String and return the entire String. For example, if given
		the argument “julie,” it will return “Julie.”
-}
capOne "" = ""
capOne (x:xs) = (toUpper x):xs
{-
		4. Now make a new version of that function that is recursive
		such that if you give it the input “woot” it will holler back
		at you “WOOT.” The type signature won’t change, but
		you will want to add a base case.
-}
capAll "" = ""
capAll (x:xs) = (toUpper x):(capAll xs)
{-
		5. To do the final exercise in this section, we’ll need another
		standard function for lists called head. Query the type of
		head and experiment with it to see what it does. Now write
		a function that will capitalize the first letter of a String
		and return only that letter as the result.
		6. Cool. Good work. Now rewrite it as a composed function.
		Then, for fun, rewrite it pointfree.
-}
cappedHead = toUpper . head
