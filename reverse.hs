module Rvrs where

rvrs :: String -> String
rvrs s = x ++ " " ++ y ++ z
  where x = take 6 (drop 10 s)
        y = take 5 (drop 5 s)
        z = take 4 s
