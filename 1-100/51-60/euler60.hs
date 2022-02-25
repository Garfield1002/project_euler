import Data.List

{-
Idea

Compute the list of tuples (p, l) where p is a suitable list
and every element in l is a prime that can be added to p, to form
a suitable list.

Suitable here refers to a list in which any two elements can be
concatenated in any order to form a prime.

This programs needs to be compiled and took around 6 seconds to execute.
-}

primes :: [Int]
primes = 2: 3: sieve (tail primes) [5,7..]

sieve :: Integral a => [a] -> [a] -> [a]
sieve ~(p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /= 0]
  where (h,~(_:t)) = span (< p*p) xs

isPrime :: Int -> Bool
isPrime n = isPrime' primes where isPrime' ~(p:ps) = p * p > n || (rem n p /= 0 && isPrime' ps)


predicate :: Int -> Int -> Bool
predicate a b = isPrime ab && isPrime ba
            where ab = read (show a ++ show b)
                  ba = read (show b ++ show a)

canAdd :: Foldable t => t Int -> Int -> Bool
canAdd t n = foldr (\ h -> (&&) (predicate h n)) True t

groupPrimes :: [([Int], [Int])] -> [([Int], [Int])]
groupPrimes []      = []
groupPrimes ((_, []):tt) = groupPrimes tt
groupPrimes ((p, h:t):tt) =
  let pp = h:p in (pp, filter (canAdd pp) t) : groupPrimes ((p, t):tt)

euler60 :: Int
euler60 = minimum $ map (\ (a, b) -> sum a + sum b)
        . filter (not . null . snd)
        . groupPrimes
        . filter ((>= 2) . length . snd)
        . groupPrimes
        . filter ((>= 3) . length . snd)
        . groupPrimes
        . filter ((>= 4) . length . snd)
        . groupPrimes
        $ [([], take 1500 primes)]

main :: IO ()
main = print euler60
