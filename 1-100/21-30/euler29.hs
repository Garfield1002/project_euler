import Data.List (nub)
euler29 = length $ nub [a ^ b | a <- [2..100], b <- [2..100]]