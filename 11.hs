{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Char
import Data.List
import Data.Maybe
{-
		Exercises: Dog Types
		Given the datatypes defined in the above sections,
		1. Is Doggies a type constructor or a data constructor?
type
		2. What is the kind of Doggies?
* -> *
		3. What is the kind of Doggies String?
*
		4. What is the type of Husky 10?
Doggies Int
		5. What is the type of Husky (10 :: Integer)?
Doggies Integer
		6. What is the type of Mastiff "Scooby Doo"?
Doggies String
		7. Is DogueDeBordeaux a type constructor or a data constructor?
both
		8. What is the type of DogueDeBordeaux?
DogueDeBordeaux :: a -> DogueDeBordeaux a
		9. What is the type of DogueDeBordeaux "doggie!"
-}
data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Tata | Mazda | Mini deriving (Eq, Show)
data Size = Size Integer deriving (Eq, Show)
data Airline = PapuAir | Catapults'RUs | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price
             | Plane Airline Size
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 10)

{-
		1. What is the type of myCar?
myCar :: Vehicle
		2. Given the following, define the functions:
		isCar :: Vehicle -> Bool
		isCar = undefined
		isPlane :: Vehicle -> Bool
		isPlane = undefined
		areCars :: [Vehicle] -> [Bool]
		areCars = undefined
-}
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar 
{-
		3. Now we’re going to write a function to tell us the manufacturer
		of a piece of data:
		getManu :: Vehicle -> Manufacturer
		getManu = undefined
-}
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

{-
		4. Given that we’re returning the Manufacturer, what will happen
		if you use this on Plane data?
type error
		5. All right. Let’s say you’ve decided to add the size of the
		plane as an argument to the Plane constructor. Add that
		to your datatypes in the appropriate places and change
		your data and functions appropriately.
ok

		Exercises: Cardinality
		While we haven’t explicitly described the rules for calculating
		the cardinality of datatypes yet, you might already have an idea
		of how to do it for simple datatypes with nullary constructors.
		Try not to overthink these exercises — follow your intuition
		based on what you know.
		1. data PugType = PugData
nullary
		2. For this one, recall that Bool is also defined with the |:
		data Airline =
		PapuAir
		| CatapultsR'Us
		| TakeYourChancesUnited
3
		3. Given what we know about Int8, what’s the cardinality of
		Int16?
65536
		4. Use the REPL and maxBound and minBound to examine Int
		and Integer. What can you say about the cardinality of
		those types?
Integer appears to be infinite. Int has a high but not unlimited maxBound
		5. Extra credit (impress your friends!): What’s the connection
		between the 8 in Int8 and that type’s cardinality of
		256?
it's the power of 2 to which the arity is raised
		-}

data Example = MakeExample deriving Show

{-
		Exercises: For Example
		1. You can query the type of a value in GHCi with the :type
		command, also abbreviated :t. Example:
		Prelude> :t False
		False :: Bool
		What is the type of data constructor MakeExample? What
		happens when you request the type of Example?
MakeExample :: Example
		2. What if you try :info on Example in GHCi? Can you determine
		what typeclass instances are defined for the Example
		type using :info in GHCi?
"Not in scope: data constructor `Example'"
		3. Try making a new datatype like Example but with a single
		type argument added to MakeExample, such as Int. What has
		changed when you query MakeExample with :type in GHCi?
-}
-- data NewEx = MakeExample String
{-
MakeExample :: String -> NewEx  -- !!
		-}

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats =
  Goats Int deriving (Eq, Show, TooMany)

{-
		Exercises: Logic Goats
		1. Reusing the TooMany typeclass, write an instance of the
		typeclass for the type (Int, String). This will require
		adding a language pragma named FlexibleInstances if
		you do not use a newtype — GHC will tell you what to do.
-}
instance TooMany (Int, String) where
  tooMany (n, _) = n > 42
{-
		2. Make another TooMany instance for (Int, Int). Sum the
		values together under the assumption this is a count of
		goats from two fields.
-}

instance TooMany (Int, Int) where
  tooMany (n, m) = n + m > 43

{-
		Exercises: Pity the Bool
		1. Given a datatype
		data BigSmall =
		Big Bool
		| Small Bool
		deriving (Eq, Show)
		What is the cardinality of this datatype? Hint: We already
		know Bool’s cardinality. Show your work as demonstrated
		earlier.
4
		2. Given a datatype
		-- needed to have Int8 in scope
		import Data.Int
		data NumberOrBool =
		Numba Int8
		| BoolyBool Bool
		deriving (Eq, Show)
		-- Example use of Numba, parentheses due to
		-- syntactic collision between (-) minus and
		-- the negate function
		let myNumba = Numba (-128)
		What is the cardinality of NumberOrBool? What happens if
258
		you try to create a Numba with a numeric literal larger than
		127? And with a numeric literal smaller than (-128)?
Won't let you break Int
		If you choose (-128) for a value precisely, you’ll notice
		you get a spurious warning:
		Prelude> let n = Numba (-128)
		Literal 128 is out of the Int8 range -128..127
		If you are trying to write a large negative
		literal, use NegativeLiterals
		Now, since -128 is a perfectly valid Int8 value you could
		choose to ignore this. What happens is that (-128) desug-
		ars into (negate 128). The compiler sees that you expect
		the type Int8, but Int8’s max boundary is 127. So even
		though you’re negating 128, it hasn’t done that step yet
		and immediately whines about 128 being larger than 127.
		One way to avoid the warning is the following:
		Prelude> let n = (-128)
		Prelude> let x = Numba n
		Or you can use the NegativeLiterals extension as it recommends:
		Prelude> :set -XNegativeLiterals
		Prelude> let n = Numba (-128)
		Note that the negative literals extension doesn’t prevent
		the warning if you use negate.
		-}
--data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth deriving (Eq,Show)
--data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq,Show)
-- cardinality 3*3=9

--data Person = MkPerson String Int deriving (Eq, Show)
-- but also
data Person = 
  Person { name :: String
         , age :: Int }
         deriving (Eq, Show)

-- data Fiction = Fiction deriving Show
-- data Nonfiction = Nonfiction deriving Show

-- data BookType = FictionBook Fiction | 
                -- NonfictionBook Nonfiction
                -- deriving (Eq, Show)

type AuthorName = String
data Author = Fiction AuthorName
              | Nonfiction AuthorName
              deriving (Eq, Show)

{-
		Exercises: How Does Your Garden Grow?
		1. Given the type
		data FlowerType = Gardenia
		| Daisy
		| Rose
		| Lilac
		deriving Show
		type Gardener = String
		data Garden =
		Garden Gardener FlowerType
		deriving Show
		What is the normal form of Garden?
-}
data GuessWhat = ChickenButt deriving (Eq, Show)

data OS =
       GnuPlusLinux
     | OpenBSD
     | Mac
     | Windows
     deriving (Eq, Show)

data PL = 
       Haskell
     | Agda
     | Idris
     | PureScript
     deriving (Eq, Show)

data Programmer = 
  Programmer { os :: OS
             , pl :: PL }
  deriving (Eq, Show)

{-
		Exercise: Programmers
		Write a function that generates all possible values of Programmer.
		Use the provided lists of inhabitants of OperatingSystem and
		ProgrammingLanguage
-}

allProgrammers :: [OS] -> [PL] -> [Programmer]
allProgrammers osl pll = [Programmer o p | o <- osl, p <- pll]

pls = [Haskell, Idris, Agda, PureScript]
oss = [GnuPlusLinux, OpenBSD, Mac, Windows ]

-- λ> length (allProgrammers oss pls) == length oss * length pls
-- True


-- > length allProgrammers oss pls == length pls * length oss #=> True

{-
		Exponentiation in what order?
		Consider the following function:
		convert :: Quantum -> Bool
		convert = undefined
		According to the equality of a -> b and 𝑏
		u�
		there should be 2
		3
		or 8 implementations of this function. Does this hold? Write
		it out and prove it for yourself.
-}
data Quantum = QuantumYes | QuantumNo | QuantumBoth

{-
convert :: Quantum -> Bool

#1
convert QuantumYes = True
convert QuantumNo = True
convert QuantumBoth = True

#2
convert QuantumYes = True
convert QuantumNo = True
convert QuantumBoth = False

#3
convert QuantumYes = True
convert QuantumNo = False
convert QuantumBoth = True

#4
convert QuantumYes = True
convert QuantumNo = False
convert QuantumBoth = False

-- etc...


		Exercises: The Quad
		Determine how many unique inhabitants each type has.
		Suggestion: just do the arithmetic unless you want to verify.
		Writing them out gets tedious quickly.
		1. data Quad =
		One
		| Two
		| Three
		| Four
		deriving (Eq, Show)
		-- how many different forms can this take?
		eQuad :: Either Quad Quad
		eQuad = ???
4^2
		2. prodQuad :: (Quad, Quad)
4^2
		3. funcQuad :: Quad -> Quad
4^4
		4. prodTBool :: (Bool, Bool, Bool)
2^3
		5. gTwo :: Bool -> Bool -> Bool
2^2^2
		6. Hint: 5 digit number
		fTwo :: Bool -> Quad -> Quad
2^4^4

-}

data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) 
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

		{- 
		Write map for BinaryTree
		Given the definition of BinaryTree above, write a map function
		for the data structure. You don’t really need to know anything
		about binary trees to write these functions. The structure
		inherent in the definition of the type is all you need. Just write
		the recursive functions and get it done.
		No special algorithms are needed, and we don’t expect you
		to keep the tree balanced or ordered. Also, remember that
		we’ve never once mutated anything. We’ve only built new
		values from input data. Given that, when you go to implement
		mapTree, you’re not changing an existing tree — you’re building
		a new one based on an existing one ( just like when you are
		mapping functions over lists).
		-}

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
-- acceptance test for mapTree
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"

-- #=> "yup okay!"

		{-Convert binary trees to lists
		Write functions to convert BinaryTree values to lists. Make
		certain your implementation passes the tests.-}

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) =
  a : (preorder left) ++ (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) =
  (inorder left) ++ a : (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = 
  (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "Bad news bears."

{-λ> testPreorder 
Preorder fine!
λ> testPostorder 
Postorder fine!
λ> testInorder 
Inorder fine!

		Write foldr for BinaryTree
		Given the definition of BinaryTree we have provided, write a
		catamorphism for the binary trees.
		-- any traversal order is fine-}

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ b Leaf = b
foldTree f b (Node left a right) =
  foldTree f (foldTree f (f a b) left) right

		{-11.18 Chapter Exercises
		Multiple choice
		1. Given the following datatype:
		data Weekday =
		Monday
		| Tuesday
		| Wednesday
		| Thursday
		| Friday
		we can say:
		a) Weekday is a type with five data constructors
		b) Weekday is a tree with five branches
		c) Weekday is a product type
		d) Weekday takes five arguments

A

		2. and with the same datatype definition in mind, what is
		the type of the following function, f?
		f Friday = "Miller Time"
		a) f :: [Char]
		b) f :: String -> String
		c) f :: Weekday -> String
		d) f :: Day -> Beer

C

		3. Types defined with the data keyword
		a) must have at least one argument
		b) must begin with a capital letter
		c) must be polymorphic
		d) cannot be imported from modules

B

		4. The function g xs = xs !! (length xs - 1)
		a) is recursive and may not terminate
		b) delivers the head of xs
		c) delivers the final element of xs
		d) has the same type as xs

C

		Ciphers
		In the Lists chapter, you wrote a Caesar cipher. Now, we want
		to expand on that idea by writing a Vigenère cipher. A Vigenère
		cipher is another substitution cipher, based on a Caesar
		cipher, but it uses a series of Caesar ciphers for polyalphabetic
		substitution. The substitution for each letter in the plaintext
		is determined by a fixed keyword.
		So, for example, if you want to encode the message “meet
		at dawn,” the first step is to pick a keyword that will determine
		which Caesar cipher to use. We’ll use the keyword “ALLY”
		here. You repeat the keyword for as many characters as there
		are in your original message:
		MEET AT DAWN
		ALLY AL LYAL
		Now the number of rightward shifts to make to encode each
		character is set by the character of the keyword that lines up
		with it. The ’A’ means a shift of 0, so the initial M will remain
		M. But the ’L’ for our second character sets a rightward shift
		of 11, so ’E’ becomes ’P’. And so on, so “meet at dawn” encoded
		with the keyword “ALLY” becomes “MPPR AE OYWY.”
		Like the Caesar cipher, you can find all kinds of resources to
		help you understand the cipher and also many examples written
		in Haskell. Consider using a combination of chr, ord, and
		mod again, possibly very similar to what you used for writing
		the original Caesar cipher.-}

caesar n s = map (chr . (+96) . flip mod 26 . (subtract 96) . (+n) . ord) s

charVig :: Char -> Char -> Char
charVig _ ' ' = ' '
charVig key c = chr . (+97) . flip mod 26 . (subtract 97) . (+ (ord key - 97)) . ord $ c
  
vig :: String -> String -> String
vig text key = [ charVig k c | (k, c) <- zip (cycle key) (concat (words text)) ]

		{-As-patterns
		“As-patterns” in Haskell are a nifty way to be able to pattern
		match on part of something and still refer to the entire original
		value. Some examples:
		f :: Show a => (a, b) -> IO (a, b)
		f t@(a, _) = do
		print a
		return t
		Here we pattern-matched on a tuple so we could get at the
		first value for printing, but used the @ symbol to introduce a
		binding named t in order to refer to the whole tuple rather
		than just a part.
		Prelude> f (1, 2)
		1
		(1,2)
		We can use as-patterns with pattern matching on arbitrary
		data constructors, which includes lists:
		doubleUp :: [a] -> [a]
		doubleUp [] = []
		doubleUp xs@(x:_) = x : xs
		Prelude> doubleUp []
		[]
		Prelude> doubleUp [1]
		[1,1]
		Prelude> doubleUp [1, 2]
		[1,1,2]
		Prelude> doubleUp [1, 2, 3]
		[1,1,2,3]
		Use as-patterns in implementing the following functions:
		1. This should return True if (and only if ) all the values in
		the first list appear in the second list, though they need
		not be contiguous.
		The following are examples of how this function should
		work:
		Prelude> isSubsequenceOf "blah" "blahwoot"
		True
		Prelude> isSubsequenceOf "blah" "wootblah"
		True
		Prelude> isSubsequenceOf "blah” "wboloath"
		True
		Prelude> isSubsequenceOf "blah" "wootbla"
		False-}

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) l2 =
  (elem x l2) && isSubsequenceOf xs l2

		{-2. Split a sentence into words, then tuple each word with the
		capitalized form of each.
		capitalizeWords :: String -> [(String, String)]
		Prelude> capitalizeWords "hello world"
		[("hello", "Hello"), ("world", "World")]-}

capitalizeWords ws =
  zip (words ws) [(toUpper . head $ a) : (tail a) | a <- (words ws)]

		{-Language exercises
		1. Write a function that capitalizes a word.
		capitalizeWord :: String -> String
		capitalizeWord = undefined
		Example output.
		Prelude> capitalizeWord "Titter"
		"Titter"
		Prelude> capitalizeWord "titter"
		"Titter"-}
cap a = (toUpper . head $ a) : (tail a) 

		{-2. Write a function that capitalizes sentences in a paragraph.
		Recognize when a new sentence has begun by checking
		for periods. Reuse the capitalizeWord function.
		capitalizeParagraph :: String -> String
		capitalizeParagraph = undefined
		Example result you should get from your function:
		Prelude> capitalizeParagraph "blah. woot ha."
		"Blah. Woot ha."-}
capitalizeParagraph :: String -> String
capitalizeParagraph p = go (words p) "" True
  where go para s willCap
         | willCap = go (tail para) (s++" "++(cap (head para))) (elem '.' (head para))
         | para == [] = drop 1 s
         | otherwise = go (tail para) (s++" "++(head para)) (elem '.' (head para))

		{-Phone exercise
		This exercise by geophf7 originally for 1HaskellADay.8 Thank
		you for letting us use this exercise!
		Remember old-fashioned phone inputs for writing text
		where you had to press a button multiple times to get different
		letters to come up? You may still have to do this when you try
		to search for a movie to watch using your television remote
		control. You’re going to write code to translate sequences of
		button presses into strings and vice versa.
		So! Here is the layout of the phone:
		-----------------------------------------
		7https://twitter.com/geophf
		8https://twitter.com/1haskelladay
		| 1 | 2 ABC | 3 DEF |
		_________________________________________
		| 4 GHI | 5 JKL | 6 MNO |
		-----------------------------------------
		| 7 PQRS | 8 TUV | 9 WXYZ |
		-----------------------------------------
		| * ^ | 0 + _ | # ., |
		-----------------------------------------
		Where star (*) gives you capitalization of the letter you’re
		writing to your friends, and 0 is your space bar. To represent
		the digit itself, you press that digit once more than the letters it
		represents. If you press a button one more than is required to
		type the digit, it wraps around to the first letter. For example,
		2 -> 'A'
		22 -> 'B'
		222 -> 'C'
		2222 -> '2'
		22222 -> 'A'
		So on and so forth. We’re going to kick this around.
		1. Create a data structure that captures the phone layout
		above. The data structure should be able to express enough
		of how the layout works that you can use it to dictate the
		behavior of the functions in the following exercises.-}

		-- fill in the rest.
-- validButtons = "1234567890*#"
type Digit = Char
data Key = Key Digit [Char]
  deriving (Eq, Show)

type DaPhone = [Key]

phoen :: DaPhone
phoen = [ 
  Key '1' [], 
  Key '2' "abc2",
  Key '3' "def3",
  Key '4' "ghi4",
  Key '5' "jkl5",
  Key '6' "mno6",
  Key '7' "pqrs7",
  Key '8' "tuv8",
  Key '9' "wxyz9",
  Key '*' "^",
  Key '0' " +_",
  Key '#' ".," ]

		{-2. Convert the following conversations into the keypresses
		required to express them. We’re going to suggest types
		and functions to fill in order to accomplish the goal, but
		they’re not obligatory. If you want to do it differently…you
		do you.-}

convo :: [String]
convo = ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol lol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Haha thanks just making sure rofl ur turn" ]

type Presses = Int
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps ((Key chr chars):phone) letter 
          | isUpper letter = ('*',1):(reverseTaps phone (toLower letter))
          | elem letter chars = [(chr, (fromJust (elemIndex letter chars))+1)]
          | otherwise = reverseTaps phone letter

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
cellPhonesDead :: DaPhone
  -> String
  -> [(Digit, Presses)]
cellPhonesDead phone =
  concatMap (reverseTaps phone)
		 --3. How many times do digits need to be pressed for each
		 --message?

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (+) 0 . map snd

		{-4. What was the most popular letter for each message? What
		was its cost? You’ll want to combine reverseTaps and fingerTaps
		to figure out what it cost in taps. reverseTaps is a list be-
		cause you need to press a different button in order to get
		capitals.-}
mostPopularLetter :: String -> Char
mostPopularLetter = fst . maximumBy (\(_, a) (_, b) -> compare a b) . cellPhonesDead phoen

		{-Hutton’s Razor
		Hutton’s Razor9
		is a very simple expression language that
		expresses integer literals and addition of values in that expression
		language. The “trick” to it is that it’s recursive and the
		two expressions you’re summing together could be literals or
		themselves further addition operations. This sort of datatype
		is stereotypical of expression languages used to motivate ideas
		in research papers and functional pearls. Evaluating or folding
		a datatype is also in some sense what you’re doing most of the
		time while programming anyway.
		9 http://www.cs.nott.ac.uk/~pszgmh/bib.html#semantics
		1. Your first task is to write the “eval” function which reduces
		an expression to a final sum.-}
data Expr
  = Lit Integer
    | Add Expr Expr
eval :: Expr -> Integer
eval (Lit int) = int
eval (Add e1 e2) = (eval e1) + (eval e2)

		{-Example of expected output:
		Prelude> eval (Add (Lit 1) (Lit 9001))
		9002
		2. Write a printer for the expressions.-}
printExpr :: Expr -> String
printExpr (Lit int) = show int
printExpr (Add e1 e2) = (printExpr e1) ++ " + " ++ (printExpr e2)

		{-Expected output:
		Prelude> printExpr (Add (Lit 1) (Lit 9001))
		"1 + 9001"
		Prelude> let a1 = Add (Lit 9001) (Lit 1)
		Prelude> let a2 = Add a1 (Lit 20001)
		Prelude> let a3 = Add (Lit 1) a2
		Prelude> printExpr a3
		"1 + 9001 + 1 + 20001"-}


