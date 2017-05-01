--data DayOfWeek = 
  --Mon | Tue | Wed | Thu | Fri | Sat | Sun

--data Date =
  --Date DayOfWeek Int
  ------deriving Show

--instance Eq DayOfWeek where
  --(==) Mon Mon = True
  --(==) Tue Tue = True
  --(==) Wed Wed = True
  --(==) Thu Thu = True
  --(==) Fri Fri = True
  --(==) Sat Sat = True
  --(==) Sun Sun = True
  --(==) _ _ = False

--instance Eq Date where
  --(==) (Date weekday dayOfMonth)
       --(Date weekday' dayOfMonth') =
    --weekday == weekday' && dayOfMonth == dayOfMonth'

data Identity a =
  Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

-- exes 6.1
-- data TisAnInteger =
--   TisAn Integer
-- instance Eq TisAnInteger where
--   (==) (TisAnInteger tis int)
--        (TisAnInteger tis' int') = 
--     tis == tis && int == int

-- data TwoIntegers =
--  Two Integer Integer
-- instance Eq TwoIntegers where
--  (==) (TwoIntegers t i j)
--       (TwoIntegers t' i' j') =
--    t == t' && i == i' && j == j'

-- data StringOrInt =
--    TisAnInt Int
--  | TisAString String
-- instance Eq StringOrInt where
--  (==) (StringOrInt soi) (StringOrInt soi') =
--    soi == soi'

-- data Pair a =
--   Pair a a
-- instance Eq Pair where
--   (==) (Pair a) (Pair b) =
--     a == b

-- etc.

data DayOfWeek = 
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show, Eq)

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ

-- 6.14 chap exes
-- 1. Eq: c. makes equality tests possible
-- 2. Ord: b. is a SUPERclass of Eq
-- 3. If Ord has operator >, > :: ?: a. Ord a => a -> a -> Bool
-- 4. x = divMod 16 12: c. x :: ()
-- 5. Integral includes: a. Int and Integer
-- Do it typecheck like it is?
-- data Person = Person Bool: does not typecheck b.c no instance of Show.
-- data Mood = Blah | Woot deriving Show: duh
--   need to derive Eq for checking "if x == Woot" 
--   function accepts Moods because (==) Woot :: (Mood x, Eq x) => x -> Bool
--   > doesn't work because Ord is a SUPERCLASS of Eq
-- Sentence Subject Verb Object typechecks
--λ> data Rocks = Rocks String deriving (Eq, Show)
--λ> data Yeah = Yeah Bool deriving (Eq, Show)
--λ> data Papu = Papu Rocks Yeah deriving (Eq, Show)
--λ> Papu "papu" False -- does not compile!! needs (Rocks "papu") (Yeah False)
-- Also needs deriving Ord before can use > and friends

-- can you substitute the second type for the first type ?>?  ?? ?? 
-- i :: Num a => a
-- i :: a

-- i = 1
-- i'm guessing you can 
-- you can NOT 
-- no instance for (Num a) arising from literal 1

-- f :: Float
-- f :: Num a => a

-- f =  1.0
-- guessed wrong again!! 
-- no instance for (Fractional a) arising from literal 1.0
-- why is 1.0 fractional???

-- f :: Float
-- f :: Fractional a => a

-- duhh hehehehe im gonnoa say YES
-- ir worked

-- f :: RealFrac a=>a 
--- im goinan say YEAS because float ha sa Real typeclass
-- it worked

-- freud :: a -> a
-- freud x= x
-- erorokdkddsd for Ord a=> workds fined

-- also workds for Int -> Int
{- 
asdf
sdf 

a) myX = 1 :: Int
sigmund :: Int -> Int
sigmund x = myX
b) sigmund :: a -> a

Will not work because Int constraint

a) myX = 1 :: Int
sigmund' :: Int -> Int
sigmund' x = myX
b) sigmund' :: Num a => a -> a

WOuld work because Num ahs Int typeclass
YIKES IT FAILED!!!!

a) You’ll need to import sort from Data.List.
jung :: Ord a => [a] -> a
jung xs = head (sort xs)
b) jung :: [Int] -> Int

Works

a) young :: [Char] -> Char
young xs = head (sort xs)
b) young :: Ord a => [a] -> a

ALSO works 

a) mySort :: [Char] -> [Char]
mySort = sort
signifier :: [Char] -> Char
signifier xs = head (mySort xs)
b) signifier :: Ord a => [a] -> a

WOILNT WORK !! mySort is [Char] only budd
-}

