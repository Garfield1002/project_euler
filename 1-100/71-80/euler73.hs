import Control.Parallel.Strategies (parList, rdeepseq, using)

euler73 :: Int -> Int
euler73 d = euler73' n
  where
    euler73' n
      | n >= nMax = 0
      | g == 1 = 1 + euler73' (n + 1)
      | otherwise = euler73' (n + 1)
      where
        g = gcd n d
    n = quot d 3 + 1
    nMax = quot d 2 + 1

main = print $ sum l
  where
    l = [euler73 d | d <- [4 .. 12000]] `using` parList rdeepseq
