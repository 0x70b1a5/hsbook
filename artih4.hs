module Arith4 where
roundtrip :: (Show a, Read a) => a -> a
roundtrip = read . show
main = do 
  print $ roundtrip 4
  print $ id 4
