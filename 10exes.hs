import Data.List
import Data.Time
{-
		Exercises: Understanding Folds
		1. foldr (*) 1 [1..5]
		will return the same result as which of the following:
		a) flip (*) 1 [1..5]
		b) foldl (flip (*)) 1 [1..5]
		c) foldl (*) 1 [1..5]
b & c
		2. Write out the evaluation steps for
		foldl (flip (*)) 1 [1..3]
(1 * 1) * 
        (2 * 1) * 
                (3 * 1)
		3. One difference between foldr and foldl is:
		a) foldr, but not foldl, traverses the spine of a list from
		right to left
		b) foldr, but not foldl, always forces the rest of the fold
		c) foldr, but not foldl, associates to the right
		d) foldr, but not foldl, is recursive
c
		4. Folds are catamorphisms, which means they are generally
		used to
		a) reduce structure
		b) expand structure
		c) render you catatonic
		d) generate infinite data structures
a
		5. The following are simple folds very similar to what you’ve
		already seen, but each has at least one error. Please fix
		them and test in your REPL:
		a) foldr (++) ["woot", "WOOT", "woot"]
foldr (++) "wut" [...
		b) foldr max [] "fear is the little death"
foldr max 'a' "f...
		c) foldr and True [False, True]
foldr (&&) True [...
		d) This one is more subtle than the previous. Can it ever
		return a different answer?
		foldr (||) True [False, True]
no, || stops at first True argument
		e) foldl ((++) . show) "" [1..5]
foldl (flip ((++) . show)) "...
		f ) foldr const 'a' [1..5]
foldr (flip const) '...
		g) foldr const 0 "tacos"
foldr (flip const) 0...
		h) foldl (flip const) 0 "burritos"
foldl const ...
		i) foldl (flip const) 'z' [1..5]
foldl const ...
-}

{-
		Exercises: Database Processing
		Write the following functions for processing this data.
-}
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
    (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
        (fromGregorian 1921 5 1)
        (secondsToDiffTime 34123))
  ]
{-
		1. Write a function that filters for DbDate values and returns
		a list of the UTCTime values inside them.
		filterDbDate :: [DatabaseItem] -> [UTCTime]
		filterDbDate = undefined
-}
isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _ = False
dbDateFromDbItem :: DatabaseItem -> UTCTime
dbDateFromDbItem (DbDate a) = a
filterDbDate db = filter isDbDate db
{-
		2. Write a function that filters for DbNumber values and returns
		a list of the Integer values inside them.
		filterDbNumber :: [DatabaseItem] -> [Integer]
		filterDbNumber = undefined
-}
isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True
isDbNumber _ = False
dbNumberFromDbItem :: DatabaseItem -> Integer
dbNumberFromDbItem (DbNumber a) = a
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = map dbNumberFromDbItem (filter isDbNumber db)
{-
		3. Write a function that gets the most recent date.
		mostRecent :: [DatabaseItem] -> UTCTime
		mostRecent = undefined
-}
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = dbDateFromDbItem (maximum (filter isDbDate db))
{-
		4. Write a function that sums all of the DbNumber values.
		sumDb :: [DatabaseItem] -> Integer
		sumDb = undefined
-}
sumDb :: [DatabaseItem] -> Integer
sumDb db = sum (filterDbNumber db)
{-
		5. Write a function that gets the average of the DbNumber values.
		-- You'll probably need to use fromIntegral
		-- to get from Integer to Double.
		avgDb :: [DatabaseItem] -> Double
		avgDb = undefined
-}
avgDb :: [DatabaseItem] -> Double
avgDb db = (fromIntegral (sumDb db))/(fromIntegral (length (filterDbNumber db)))

{-
		Scans Exercises
		1. Modify your fibs function to only return the first 20 Fibonacci
		numbers.
		2. Modify fibs to return the Fibonacci numbers that are less
		than 100.
		3. Try to write the factorial function from Recursion as a
		scan. You’ll want scanl again, and your start value will be
		1. Warning: this will also generate an infinite list, so you
		may want to pass it through a take function or similar.
		CHAPTER 10. DATA STRUCTURE ORIGAMI 566
		10.10 Chapter Exercises
		Warm-up and review
		For the following set of exercises, you are not expected to use
		folds. These are intended to review material from previous
		chapters. Feel free to use any syntax or structure from previous
		chapters that seems appropriate.
		1. Given the following sets of consonants and vowels:
		stops = "pbtdkg"
		vowels = "aeiou"
		a) Write a function that takes inputs from stops and
		vowels and makes 3-tuples of all possible stop-vowelstop
		combinations. These will not all correspond to
		real words in English, although the stop-vowel-stop
		pattern is common enough that many of them will.
		b) Modify that function so that it only returns the combinations
		that begin with a p.
		c) Now set up lists of nouns and verbs (instead of stops
		and vowels) and modify the function to make tuples
		representing possible noun-verb-noun sentences.
		2. What does the following mystery function do? What is
		its type? Try to get a good sense of what it does before
		you test it in the REPL to verify it.
		CHAPTER 10. DATA STRUCTURE ORIGAMI 567
		seekritFunc x =
		div (sum (map length (words x)))
		(length (words x))
		3. We’d really like the answer to be more precise. Can you
		rewrite that using fractional division?
		Rewriting functions using folds
		In the previous chapter, you wrote these functions using direct
		recursion over lists. The goal now is to rewrite them using
		folds. Where possible, to gain a deeper understanding of
		folding, try rewriting the fold version so that it is point-free.
		Point-free versions of these functions written with a fold
		should look like:
		myFunc = foldr f z
		So for example with the and function:
		CHAPTER 10. DATA STRUCTURE ORIGAMI 568
		-- Again, this type will be less reusable than
		-- the one in GHC 7.10 and newer. Don't worry.
		-- direct recursion, not using (&&)
		myAnd :: [Bool] -> Bool
		myAnd [] = True
		myAnd (x:xs) =
		if x == False
		then False
		else myAnd xs
		-- direct recursion, using (&&)
		myAnd :: [Bool] -> Bool
		myAnd [] = True
		myAnd (x:xs) = x && myAnd xs
		-- fold, not point-free in the folding function
		myAnd :: [Bool] -> Bool
		myAnd = foldr
		(\a b ->
		if a == False
		then False
		else b) True
		-- fold, both myAnd and the folding function are point-free now
		myAnd :: [Bool] -> Bool
		myAnd = foldr (&&) True
		CHAPTER 10. DATA STRUCTURE ORIGAMI 569
		The goal here is to converge on the final version where
		possible. You don’t need to write all variations for each example,
		but the more variations you write, the deeper your
		understanding of these functions will become.
		1. myOr returns True if any Bool in the list is True.
		myOr :: [Bool] -> Bool
		myOr = undefined
		2. myAny returns True if a -> Bool applied to any of the values
		in the list returns True.
		myAny :: (a -> Bool) -> [a] -> Bool
		myAny = undefined
		Example for validating myAny:
		Prelude> myAny even [1, 3, 5]
		False
		Prelude> myAny odd [1, 3, 5]
		True
		3. Write two versions of myElem. One version should use
		folding and the other should use any.
		myElem :: Eq a => a -> [a] -> Bool
		CHAPTER 10. DATA STRUCTURE ORIGAMI 570
		Prelude> myElem 1 [1..10]
		True
		Prelude> myElem 1 [2..10]
		False
		4. Implement myReverse, don’t worry about trying to make
		it lazy.
		myReverse :: [a] -> [a]
		myReverse = undefined
		Prelude> myReverse "blah"
		"halb"
		Prelude> myReverse [1..5]
		[5,4,3,2,1]
		5. Write myMap in terms of foldr. It should have the same
		behavior as the built-in map.
		myMap :: (a -> b) -> [a] -> [b]
		myMap = undefined
		6. Write myFilter in terms of foldr. It should have the same
		behavior as the built-in filter.
		myFilter :: (a -> Bool) -> [a] -> [a]
		myFilter = undefined
		CHAPTER 10. DATA STRUCTURE ORIGAMI 571
		7. squish flattens a list of lists into a list
		squish :: [[a]] -> [a]
		squish = undefined
		8. squishMap maps a function over a list and concatenates the
		results.
		squishMap :: (a -> [b]) -> [a] -> [b]
		squishMap = undefined
		Prelude> squishMap (\x -> [1, x, 3]) [2]
		[1,2,3]
		Prelude> squishMap (\x -> "WO " ++ [x] ++ " OT ") "blah"
		"WO b OT WO l OT WO a OT WO h OT "
		9. squishAgain flattens a list of lists into a list. This time re-use
		the squishMap function.
		squishAgain :: [[a]] -> [a]
		squishAgain = undefined
		10. myMaximumBy takes a comparison function and a list and
		returns the greatest element of the list based on the last
		value that the comparison returned GT for.
		myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
		myMaximumBy = undefined
		CHAPTER 10. DATA STRUCTURE ORIGAMI 572
		Prelude> myMaximumBy (\_ _ -> GT) [1..10]
		1
		Prelude> myMaximumBy (\_ _ -> LT) [1..10]
		10
		Prelude> myMaximumBy compare [1..10]
		10
		11. myMinimumBy takes a comparison function and a list and
		returns the least element of the list based on the last value
		that the comparison returned LT for.
		myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
		myMinimumBy = undefined
		Prelude> myMinimumBy (\_ _ -> GT) [1..10]
		10
		Prelude> myMinimumBy (\_ _ -> LT) [1..10]
		1
		Prelude> myMinimumBy compare [1..10]
		1
-}
