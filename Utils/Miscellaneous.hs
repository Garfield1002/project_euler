module Utils.Miscellaneous where

import qualified Data.List as L

-- ============================================================================
--                                Extra Maths
-- ============================================================================

-- | The `combinations n k` function computes the amount of combinations of `k` elements from a set of `n` elements.
combinations :: Integral a => a -> a -> a
combinations n k = quot (product [n - k + 1 .. n]) (product [1 .. k])

-- ============================================================================
--                                Operations on Lists
-- ============================================================================

-- | The `permutations` function computes the permutations of a list.
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (h : t) = concatMap (insertH []) (permutations t)
  where
    insertH l [] = [h : l]
    insertH l (h' : t') = (h' : t' ++ h : l) : insertH (h' : l) t'

-- ============================================================================
--                                Operations on Numbers
-- ============================================================================

-- | The `listDigits` function computes the digits of a number represented as a list.
--
-- ⚠ This list is reversed.
listDigits :: Integral a => a -> [a]
listDigits n
  | q == 0 = [r]
  | otherwise = r : listDigits q
  where
    (q, r) = divMod n 10

-- | The `intSqrt` function computes the integer square root of an integer \(n\)
--
-- The integer square root is defined as \(isqrt(n) = \lfloor \sqrt{n} \rfloor \)
intSqrt :: Integral t => t -> t
intSqrt n
  | n < 0 = error "Negative input"
  | n == 0 = 0
  | otherwise = babylon n
  where
    babylon a
      | a > b = babylon b
      | otherwise = a
      where
        b = quot (a + quot n a) 2
