import Data.Set ( Set, fromList, member)
import Data.List (nub)

primeList :: [Int]
primeList = 2: 3: sieve (tail primeList) [5,7..]

sieve :: [Int] -> [Int] -> [Int]
sieve ~(p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /= 0]
  where (h,~(_:t)) = span (< p*p) xs

primes :: [Int]
primes = dropWhile (< 10^3) . takeWhile (< 10^4) $ primeList

listDigits :: Integral t => t -> [t]
listDigits = reverse . listDigits'
    where listDigits' n
            | q == 0    = [r]
            | otherwise = r:listDigits' q
            where (q, r) = divMod n 10

intList :: Num p => [p] -> p
intList = intList' . reverse
    where   intList' [] = 0
            intList' (h:t) = h + 10 * intList' t

permutations :: [a] -> [[a]]
permutations l = permutation' l []
        where permutation' [] e = [[]]
              permutation' [a] e = map (a:) (permutations e)
              permutation' (h:t) e =
                  map (h:) (permutations (e ++ t)) ++ permutation' t (e ++ [h])

increasing :: Ord a => [a] -> Bool
increasing (x:y:xs) = x < y && increasing (y:xs)
increasing _ = True

arithmetic :: (Eq a, Num a) => [a] -> Bool
arithmetic l = arithmetic' (tail l)
    where arithmetic' (x:xs) = elem (2 * x -  head l) xs || arithmetic' xs
          arithmetic' _ = False

euler49 = filter arithmetic . filter ((>=3) . length) . map (filter (`member` set) . nub . map intList . permutations . listDigits) $ primes
            where set = fromList primes