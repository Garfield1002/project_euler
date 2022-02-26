module Utils.Miscellaneous where

{-
The digits of a number represented as a list.

⚠ This list is reversed.

> listDigits 123
  [3, 2, 1]
-}
listDigits :: Integral a => a -> [a]
listDigits n
    | q == 0    = [r]
    | otherwise = r:listDigits q
    where (q, r) = divMod n 10