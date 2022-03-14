import Data.Array (listArray, (!))
import Utils.Primes (primes)

-- The amount of proper fractions with denominator n > 1 is equal to the amount of numbers lesser than n and relatively prime with n.
-- This just so happens to be the value of euler's totient function

euler72 n = sum phis - 1
  where
    go 1 _ = 1
    go p [] = p - 1
    go d (p : ps)
      | r == 0 && r' == 0 = p * phis ! q
      | r == 0 = (p - 1) * phis ! q
      | otherwise = go d ps
      where
        (q, r) = quotRem d p
        r' = rem q p
    phis = listArray (1, n) [go x $ takeWhile ((<= x) . (^ 2)) primes | x <- [1 .. n]]

main = print . euler72 $ 10 ^ 6
