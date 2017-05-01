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

