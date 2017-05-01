data Content = Powerful | Weak deriving Show
catalyzeContent :: Content -> Content
catalyzeContent Weak = Powerful
catalyzeContent _ = Weak

-- exercises
-- not True && True -- 'true'
-- not (x == 6) -- '='
-- (1*2)>5
-- "Merry" > "Happy" -- [] -> ""
-- ['1','2','3'] ++ "ayy lmao"

logOnLine :: String -> IO()
logOnLine username = 
  if isMe username
    then putStrLn "Powerful content coming up"
  else
    putStrLn "Zzzzzz"
  where isMe username = username == "0x70b1a5"

--- exercises
a = [1,2,3]
b = [2,4,5,6]
c = [a,b]
-- length c -- 2
-- length(concat c) -- 7
-- 3 / length c fails. why
-- -> / needs Fractional, length returns Int
-- 3 `div` c -- works
isPal :: (Eq a) => [a] -> Bool
isPal x =
  if reverse(x) == x then True
  else False

myabs :: Integer -> Integer
myabs x =
  if x >= 0 then x
  else negate x

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f a b = ((snd a, snd b), (fst a, fst b))

x = (+)
f1 xs = w `x` 1
  where w = length xs

-- exercises 5.1 
-- 1a. not's typesignature is Bool -> Bool
-- 1b. length's TS is [a] -> Int
-- 1c. concat's TS is [[a]] -> [a]
-- 1d. head's TS is [a] -> a
-- 1e. (<)'s TS is Num -> Num -> Bool
-- answers:
-- 1e incorrect: (<) :: Ord a => a -> a -> Bool

-- 5.2
-- 1. if f :: a ->a ->a ->a; x :: Char
--    then f x :: ?
--   A: Char->Char->Char
-- 2. if g :: a->b->c->b
--    then g 0 'c' "woot" :: ?
--   A: Char
-- 3. h :: (Num a, Num b) => a->b->b
--    h 1.0 2 :: ?
--   A: Integer
-- 4. h :: (Num a, Num b) => a -> b -> b
--    h 1 (5.5 :: Double) :: ?
--   A: Double
-- 5. jackal :: (Ord a, Eq b) => a -> b -> a
--    jackal "keyboard" "has the word jackal in it" :: ?
--   A: [Char]
-- 6. jackal :: (Ord a, Eq b) => a -> b -> a
--    jackal "keyboard" :: ?
--   A: Eq b => b -> [Char]
-- 7. kessel :: (Ord a, Num b) => a -> b -> a 
--    kessel 1 2 :: ?
--   A: Ord a => a
-- 8. kessel :: (Ord a, Num b) => a-> b -> a
--    kessel 1 (2 :: Integer) :: ?
--   A: Ord a => a 
-- 9. kessel :: (Ord a, Num b) => a -> b -> a
--    kessel (1 :: Integer) 2 :: ?
--   A: Integer

-- INCORRECT: #3 h 1.0 2 :: Num b => b
-- INCORRECT: #8 kessel 1 (2 :: Integer) :: (Num a, Ord a) => a


-- exes 5.8
-- 1. a value of type [a] is 
-- c. a list whose elements are of some type a
-- 2. a fn of type [[a]] -> [a] could
-- a. take a list of strings as an argument
-- 3. a function of type [a] -> Int -> a
-- b. returns an element of type a from a list
-- 4. a fn of type (a,b) -> a
-- c. takes a tuple argument and returns the first value

-- Exes 5.9
-- 1. a. (*9) 6 :: Num a => a
-- b. head [(0, "doge"),(1,"kitteh")] :: (Num t, String t1) => (t, t1)
-- head [(0 :: Integer, "doge"),(1,"1")] :: (Integer t, String t1) => (t, t1)
-- if False then True else False :: Bool
-- length [1..4] :: Integer
-- (length [1..11]) < (length [1..12]) :: Bool
-- 2. Giben x=5; y=x+5; w=y*10; w :: _?
--  A: Integer
-- 3. Giben x=5; y=x+5; z y = y * 10; z :: _?
--  A: z :: Num y => y -> y
-- 4. Giben f = 4/y; f :: _?
--  A: f :: Fractional
-- Giben a="AA";b="BB";c="CC";
-- :: String

-- 5.10
-- 1. bigNum = (^) 5 $ 10
--    wagoo = bigNum $ 10 -- should caues error, (^) acepts 2 args
-- 2. let x = print
-- print "ADSFasdf" -- should cause error because x is not a function; must define as:
-- let x y = print y
-- 3. a = (+); b=5; c=b 10; d = c 200 -- uh. duh?
-- b = a 5; c = a b 10
-- 4 a = 12 + b -- immediate fail; no b
--   b = 10000 * c -- also fail
--    a b = 12 + b
--    b c = 10e5 * c

-- 5.11 Type of polymorphic
-- CP (constrained polymorphic); FP (fully polymorphic); C (concrete)
-- 1. f :: zed -> Zed -> Blah
--  A:     FP     C      C
-- 2. f :: Enum b => a -> b -> C
--  A:          CP   FP   CP   C
-- 3. f :: f -> g -> C
--         FP   FP   C

-- 5.12 sdignatures
-- 1. fH (x:_) = x
--  A: :: [a] -> a
-- 2. fC x y = if (x>y) then True else False
--  A: :: (Ord a, Orb b) => a -> b -> Bool
-- 3. fS (x, y) = y
--  A: :: (a, b) -> b

-- 5.12 given type, write fn
-- 1. 

g a = a ++ a 
fonc o = o ++ o
     where o = [5]

-- co b2c a2b a = (b2c (a2b a))

fS :: [Char] -> [Char]
fT :: [Char] -> [Char]
fS x = x ++ " i nthe rain"
fT x = x ++ " over the rainbow"
sing = if (x<y) then fS x else fT y
  where x = "Singgin"
        y = "Somewhre"

main :: IO ()
main = do
  print (1+2)
  putStrLn "10"
  print (negate (-1))
  print ((+) 0 blah)
    where blah = negate 1

-- 5.14?5
-- f :: Int -> String
-- g :: String -> Char
-- h :: Int -> Char
-- h i = (g (f i))

-- q :: A->B
-- w :: B->C
-- e :: A->C
-- e a = (w (q a))

-- xz :: X->Z; yz :: Y->Z
-- xform :: (X,Y)->(Z,Z)
-- xform (x,y) = (xz x, yz y)

-- munge :: (x->y) -> (y->(w,z)) -> x -> w
-- munge x2y y2wz x = fst (y2wz (x2y x))
