module Utils.Primes
  ( primes,
    isPrime,
    primeFactors,
  )
where

primes :: [Integer]
primes = 2 : 3 : sieve (tail primes) [5, 7 ..]

sieve :: Integral a => [a] -> [a] -> [a]
sieve ~(p : ps) xs = h ++ sieve ps [x | x <- t, rem x p /= 0]
  where
    (h, ~(_ : t)) = span (< p * p) xs

isPrime :: Integer -> Bool
isPrime n = isPrime' primes where isPrime' ~(p : ps) = p * p > n || (rem n p /= 0 && isPrime' ps)

primeFactors :: Integer -> [Integer]
primeFactors = primeFactors' primes
  where
    primeFactors' ~(p : ps) n
      | mod n p == 0 = p : primeFactors' (p : ps) (quot n p)
      | p > n = []
      | otherwise = primeFactors' ps n
