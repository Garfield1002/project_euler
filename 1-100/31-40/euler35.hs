{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Set ( Set, fromList, member)
import Control.Parallel.Strategies ( parList, rdeepseq, using )


-- primeList :: [Integer]
-- primeList = sieve [2..]
--         where
--             sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primeList :: [Integer]
primeList = 2: 3: sieve (tail primeList) [5,7..]

sieve :: Integral a => [a] -> [a] -> [a]
sieve (p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /= 0]
  where (h,~(_:t)) = span (< p*p) xs

primeL :: [Integer]
primeL = takeWhile (<= 1000000) primeList

primes :: Set Integer
primes = fromList primeL

cPermutations :: [a] -> [[a]]
cPermutations = cPermutations' []
    where cPermutations' _ []       = []
          cPermutations' p (h:t)    = (h:t ++ p):cPermutations' (p ++ [h]) t

listDigits :: Integral t => t -> [t]
listDigits n
    | q == 0    = [r]
    | otherwise = r:listDigits q
    where (q, r) = divMod n 10

intList :: Num p => [p] -> p
intList [] = 0
intList (h:t) = h + 10 * intList t

isCircular :: Integer -> Bool
isCircular p = all (`member` primes) $ tail . map intList $ cPermutations $ listDigits p

euler35 :: Int
euler35 = length l where l = [p | p <- primeL, isCircular p] `using` parList rdeepseq

main = print euler35
