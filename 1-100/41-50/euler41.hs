{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Set ( fromList, member, Set )

primes :: [Int]
primes = 2: 3: sieve (tail primes) [5,7..]

sieve :: Integral a => [a] -> [a] -> [a]
sieve (p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /= 0]
  where (h,~(_:t)) = span (< p*p) xs

isPrime :: Int -> Bool
isPrime n = isPrime' primes where isPrime' ~(p:ps) = p * p > n || (rem n p /= 0 && isPrime' ps)

intList :: Num p => [p] -> p
intList [] = 0
intList (h:t) = h + 10 * intList t

permutations :: [a] -> [[a]]
permutations l = permutation' l []
        where permutation' [] e = [[]]
              permutation' [a] e = map (a:) (permutations e)
              permutation' (h:t) e =
                  map (h:) (permutations (e ++ t)) ++ permutation' t (e ++ [h])

pandigitals :: [Int]
pandigitals = map intList (permutations [1..8])

euler41 = maximum [p | p <- pandigitals, isPrime p]
