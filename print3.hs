module Print3 where

greeting = "Unghh"

print3 :: IO ()
print3 = do
  putStrLn greeting

doThing :: String -> Char
doThing s = s !! 3

main :: IO ()
main = do
  putStrLn greeting
  print3
