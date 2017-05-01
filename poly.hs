-- 5.5 exercises
-- 1. a -> a cna't do anything but return a so i'm not goin gto bother to with that
-- 2. a -> a -> a, how to create?
---- one way: g a b = a
---- two way (maybe?): h a = \b -> a
---- 'im also not going to bother to try to violate the parametricity constraints
-- 3. a -> b -> b
---- \a -> \b -> b (appears correct)

--f :: Num a => a -> a -> a
-- type inference
f x y = x + y + 3

-- 5.6 exes
-- 1. (++) :: [a] -> [a] -> [a]
---- mC r = r ++ "g"
---- mC :: [Char] -> [Char] -> [Char]
-- 2. (*) :: Num a => a-> a-> a
---- mM x = x/3*5
---- mM :: Fractional x => x -> x
-- 3. take :: Int -> [a] -> [a]
---- mT x = take x "the industrial revolution a"
---- mT :: (Int x, String a) => x -> a -> a
------ INCORRECT: mT :: Int -> [Char]
-- 4. (>) :: Ord a => a -> a -> Bool
---- mC r = r > (length [1..10])
---- mC :: Int -> Bool
------ INCORRECT: mC :: (Num a, Ord a) => a -> Bool
-- 5. (<) :: Ord a => a -> a -> Bool
---- mA x = x < 'z'
---- mA :: (Ord a, Char a) => a -> Bool
----- INCORRECT: mA :: Char -> Bool

