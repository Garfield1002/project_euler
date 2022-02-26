module Utils.Miscellaneous where

-- | The `listDigits` function computes the digits of a number represented as a list.
--
-- ⚠ This list is reversed.
listDigits :: Integral a => a -> [a]
listDigits n
    | q == 0    = [r]
    | otherwise = r:listDigits q
    where (q, r) = divMod n 10

-- | The `intSqrt` function computes the integer square root of an integer \(n\)
--
-- The integer square root is defined as \(isqrt(n) = \lfloor \sqrt{n} \rfloor \)
intSqrt :: Integral t => t -> t
intSqrt n
  | n < 0     = error "Negative input"
  | n == 0    = 0
  | otherwise = babylon n
  where
    babylon a
      | a > b     = babylon b
      | otherwise = a
      where b  = quot (a + quot n a) 2
