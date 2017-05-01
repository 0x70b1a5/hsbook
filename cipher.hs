module Cipher where
import Data.Char
{-
		Ciphers
		We’ll still be using Data.Char for this next exercise. You should
		save these exercises in a module called Cipher because we’ll
		be coming back to them in later chapters. You’ll be writing a
		Caesar cipher for now, but we’ll suggest some variations on
		the basic program in later chapters.
		A Caesar cipher is a simple substitution cipher, in which
		each letter is replaced by the letter that is a fixed number of
		places down the alphabet from it. You will find variations on
		this all over the place — you can shift leftward or rightward,
		for any number of spaces. A rightward shift of 3 means that
		’A’ will become ’D’ and ’B’ will become ’E,’ for example. If you
		did a leftward shift of 5, then ’a’ would become ’v’ and so forth.
		Your goal in this exercise is to write a basic Caesar cipher
		that shifts rightward. You can start by having the number of
		spaces to shift fixed, but it’s more challenging to write a cipher
		that allows you to vary the number of shifts so that you can
		encode your secret messages differently each time.
		There are Caesar ciphers written in Haskell all over the
		internet, but to maximize the likelihood that you can write
		yours without peeking at those, we’ll provide a couple of tips.
		When yours is working the way you want it to, we would
		encourage you to then look around and compare your solution
		to others out there.
		The first lines of your text file should look like this:
        module Cipher where
		import Data.Char
		Data.Char includes two functions called ord and chr that can
		be used to associate a Char with its Int representation in the
		Unicode system and vice versa:
		*Cipher> :t chr
		chr :: Int -> Char
		*Cipher> :t ord
		ord :: Char -> Int
		Using these functions is optional; there are other ways you
		can proceed with shifting, but using chr and ord might simplify
		the process a bit.
		You want your shift to wrap back around to the beginning of
		the alphabet, so that if you have a rightward shift of 3 from ’z,’
		you end up back at ’c’ and not somewhere in the vast Unicode
		hinterlands. Depending on how you’ve set things up, this
		might be a bit tricky. Consider starting from a base character
		(e.g., ’a’) and using mod to ensure you’re only shifting over the
		26 standard characters of the English alphabet.
		You should include an unCaesar function that will decipher
		your text as well. In a later chapter, we will test it.
-}

caesar n s = map (chr . (+96) . flip mod 26 . (subtract 96) . (+n) . ord) s
uncaesar n s = caesar (-n) s
{-
		Writing your own standard functions
		Below are the outlines of some standard functions. The goal
		here is to write your own versions of these to gain a deeper
		understanding of recursion over lists and how to make functions
		flexible enough to accept a variety of inputs. You could
		figure out how to look up the answers, but you won’t do that
		because you know you’d only be cheating yourself out of the
		knowledge. Right?
		Let’s look at an example of what we’re after here. The and2
		function can take a list of Bool values and returns True if and
		only if no values in the list are False. Here’s how you might
		write your own version of it:
		-- direct recursion, not using (&&)
		myAnd :: [Bool] -> Bool
		myAnd [] = True
		myAnd (x:xs) = if x == False then False else myAnd xs
		-- direct recursion, using (&&)
		myAnd :: [Bool] -> Bool
		myAnd [] = True
		myAnd (x:xs) = x && myAnd xs
		2 Note that if you’re using GHC 7.10 or newer, the functions and, any, and all have been
		abstracted from being usable only with lists to being usable with any datatype that has an
		instance of the typeclass Foldable. It still works with lists just the same as it did before.
		Proceed assured that we’ll cover this later.
		And now the fun begins:
		1. myOr returns True if any Bool in the list is True.
		myOr :: [Bool] -> Bool
		myOr = undefined
-}
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs
{-
		2. myAny returns True if a -> Bool applied to any of the values
		in the list returns True.
		myAny :: (a -> Bool) -> [a] -> Bool
		myAny = undefined
		Example for validating myAny:
		Prelude> myAny even [1, 3, 5]
		False
		Prelude> myAny odd [1, 3, 5]
		True
-}
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = (f x) || myAny f xs
{-
		3. After you write the recursive myElem, write another version
		that uses any.
		-- the built-in version of 'elem' in GHC 7.10
		-- and newer has a type that uses Foldable
		-- instead of the list type specifically. You
		-- can ignore that and write the concrete
		-- version that works only for list.
		myElem :: Eq a => a -> [a] -> Bool
		Prelude> myElem 1 [1..10]
		True
		Prelude> myElem 1 [2..10]
		False
-}
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem i (x:xs) = (i == x) || myElem i xs
{-
		4. Implement myReverse.
		myReverse :: [a] -> [a]
		myReverse = undefined
		Prelude> myReverse "blah"
		"halb"
		Prelude> myReverse [1..5]
		[5,4,3,2,1]
-}
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs)++[x]
{-
		5. squish flattens a list of lists into a list
		squish :: [[a]] -> [a]
		squish = undefined
-}
squish = concat
{-
		6. squishMap maps a function over a list and concatenates the
		results.
		squishMap :: (a -> [b]) -> [a] -> [b]
		squishMap = undefined
		Prelude> squishMap (\x -> [1, x, 3]) [2]
		[1,2,3]
		Prelude> squishMap (\x -> "WO "++[x]++" HOO ") "123"
		"WO 1 HOO WO 2 HOO WO 3 HOO "
-}
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = (f x)++(squishMap f xs)
{-
		7. squishAgain flattens a list of lists into a list. This time re-use
		the squishMap function.
		squishAgain :: [[a]] -> [a]
		squishAgain = undefined
-}
squishAgain = squishMap id -- lol 
{-
		8. myMaximumBy takes a comparison function and a list and
		returns the greatest element of the list based on the last
		value that the comparison returned GT for. If you import
		maximumBy from Data.List, you’ll see the type is:
		Foldable t => (a -> a -> Ordering) -> t a -> a
		rather than
		(a -> a -> Ordering) -> [a] -> a
		myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
		myMaximumBy = undefined
		Prelude> let xs = [1, 53, 9001, 10]
		Prelude> myMaximumBy compare xs
		9001
-}
myMaxBy :: (a -> a -> Ordering) -> [a] -> a
myMaxBy f (x:[]) = x
myMaxBy f (x1:x2:xs) = if (f x1 x2) == GT then (myMaxBy f (x1:xs)) else (myMaxBy f (x2:xs)) 
{-
		9. myMinimumBy takes a comparison function and a list and
		returns the least element of the list based on the last value
		that the comparison returned LT for.
		myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
		myMinimumBy = undefined
		Prelude> let xs = [1, 53, 9001, 10]
		Prelude> myMinimumBy compare xs
		1
-}
myMinBy :: (a -> a -> Ordering) -> [a] -> a
myMinBy f (x:[]) = x
myMinBy f (x1:x2:xs) = if (f x1 x2) == LT then (myMinBy f (x1:xs)) else (myMinBy f (x2:xs)) 
{-
		Using the myMinimumBy and myMaximumBy functions, write your
		own versions of maximum and minimum. If you have GHC
		7.10 or newer, you’ll see a type constructor that wants a
		Foldable instance instead of a list as has been the case for
		many functions so far.
		myMaximum :: (Ord a) => [a] -> a
		myMaximum = undefined
		myMinimum :: (Ord a) => [a] -> a
		myMinimum = undefined
-}
myMax :: (Ord a) => [a] -> a
myMax = myMaxBy compare
myMin :: (Ord a) => [a] -> a
myMin = myMinBy compare
