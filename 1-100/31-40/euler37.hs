{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.Set ( fromList, member, Set )
import Control.Parallel.Strategies ( parList, rdeepseq, rpar, using )


primeList :: [Integer]
primeList = 2: 3: sieve (tail primeList) [5,7..]

sieve :: Integral a => [a] -> [a] -> [a]
sieve (p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /= 0]
  where (h,~(_:t)) = span (< p*p) xs

elemOrd :: Ord t => t -> [t] -> Bool
elemOrd n [] = False
elemOrd n (x:xs) = n == x || (x < n && elemOrd n xs)

primeL :: [Integer]
primeL = takeWhile (<= 1000000) primeList

primes :: Set Integer
primes = fromList primeL

isTrunc n = isTrunc' 10
    where isTrunc' t
            | q == 0    = True
            | member q primes && member r primes  = isTrunc' $ t * 10
            | otherwise = False
            where (q, r) = quotRem n t

main = print . sum $ l
        where l = [p | p <- primeL, isTrunc p] `using` parList rdeepseq
