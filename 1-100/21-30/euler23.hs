import Control.Parallel.Strategies ( parList, rdeepseq, using )
import Data.Array

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

divisors :: Int -> [Int]
divisors n = divisors 1 [] (mod n 2 + 1)
    where divisors i l inc
            | i > isqrt n = l
            | otherwise   =
                let (q, r) = divMod n i in
                    if r == 0 then
                        if i /= q then  divisors (i + inc) (i:q:l) inc
                        else            divisors (i + inc) (i:l) inc
                    else divisors (i + inc) l inc

d :: Int -> Bool
d i = sum (divisors i) - i > i

n :: Int
n = 28124

abundsArray :: Array Int Bool
abundsArray = listArray (1,n) $ map d [1..n]

abunds :: [Int]
abunds = filter (abundsArray !) [1..n]

rests :: Int -> [Int]
rests x = map (x-) $ takeWhile (<= div x 2) abunds

isSum :: Int -> Bool
isSum = any (abundsArray !) . rests

main = print . sum . filter (not . isSum) $ [1..n]
