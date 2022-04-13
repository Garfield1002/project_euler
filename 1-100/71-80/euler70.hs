import           Utils.Miscellaneous (num2digits)
import           Data.List (sort, sortOn)
import           Utils.Primes (primes)
import           Data.List.NonEmpty (sortWith)

-- Minimum of n/φ(n) implies using the product of primes
-- Is an int a permutation of another
isPermutation :: Integral a => a -> a -> Bool
isPermutation n1 n2 = sort (num2digits n1) == sort (num2digits n2)

isValid :: Integral i => [i] -> Bool
isValid ps = isPermutation (product ps) (product $ map (\p -> p - 1) ps)

euler70 = head . map product . sortOn foo . filter isValid
  $ [[p1, p2]
    | p1 <- takeWhile (< 10 ^ 4) primes
    , p2 <- takeWhile ((< 10 ^ 7) . (* p1)) primes]
  where
    foo ps = (fromIntegral . product $ ps)
      / (fromIntegral . product $ map (\p -> p - 1) ps)

main = print euler70