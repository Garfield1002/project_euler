{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
primeList :: [Integer]
primeList = 2: 3: sieve (tail primeList) [5,7..]

primes = map ((-1) *) p ++ p
    where p = takeWhile (<= 1000) primeList

sieve :: Integral a => [a] -> [a] -> [a]
sieve (p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /= 0]
  where (h,~(_:t)) = span (< p*p) xs

elemOrd :: Ord t => t -> [t] -> Bool
elemOrd e []    = False
elemOrd e (h:t) = h == e || (h < e && elemOrd e t)

score :: Integer -> Integer -> Integer
score a b = score' 1
    where score' n
            | elemOrd (n^2 + a * n + b) primeList = score' (n+1)
            | otherwise = n

main = print $ maximum [(s, a, b) | b <- primes, a <- [-1000..1000], let s = score a b]
