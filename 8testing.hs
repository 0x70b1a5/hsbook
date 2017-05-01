data DivResult = Result (Integer, Integer) | DivByZero deriving Show

dividedBy :: Integer -> Integer -> DivResult
dividedBy num denom = go num denom 0 False
  where go n d count neg
         | d == 0 = DivByZero
         | and [abs n < abs d, neg] = Result ((-count), n)
         | and [abs n < abs d, not neg] = Result (count, n)
         | n < 0 = go ((-n) - d) d (count + 1) True
         | d < 0 = go (n - (-d)) (-d) (count + 1) True
         | otherwise = go (n - d) d (count + 1) neg
