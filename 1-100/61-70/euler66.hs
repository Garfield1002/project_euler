import Utils.Miscellaneous (intSqrt)
import Utils.Fraction ( Fraction(Frac), cfSqrt, continuousFrac )

{-
Find solutions to the quadratic Diophantine equations of the form
x^2 - Dy^2 = 1
-}


-- | All integers that are not a square
notSquare :: Integral a => [a]
notSquare = notSquare' 2 2
        where notSquare' n i
                | n == i * i = notSquare' (n + 1) (i + 1)
                | otherwise  = n:notSquare' (n + 1) i


-- For documentation's sake, here is my initial solution function
-- It is way too slow.
-- solution :: Integer -> Integer
-- solution d = solution' 1
--     where solution' y
--             | y > 10^6      = error "Too long"
--             | x * x == x2   = x
--             | otherwise     = solution' (y + 1)
--             where   x2 = d * y^2 + 1
--                     x = intSqrt x2

-- | Solving Pell's equation at d using Continued Fractions
solution :: Integer -> (Integer, Integer)
solution d = head [ (x, y) | n <- [1..], let Frac x y = continuousFrac . take n . cfSqrt $ d, x^2 - d*y^2 == 1]

main :: IO ()
main = print . snd . maximum $ [(solution n, n) | n <- takeWhile (<=1000) notSquare]