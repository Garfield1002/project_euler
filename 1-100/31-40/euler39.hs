{-# LANGUAGE TupleSections #-}
import Data.List

genTriangle m n = (m^2 - n^2, 2 * m * n, m^2 + n^2)

allTriangles = [t | m <- [1..]
                  , n <- [1..m - 1]
                  , let t = genTriangle m n
                  , (even m || even n) && gcd m n == 1]

perimeter :: Num a => (a, a, a) -> a
perimeter (x, y, z) = x + y + z

perimeters = takeWhile (<= 1000) . nub . sort . takeWhile (< 2000) $ perimeter <$> allTriangles

f :: Integral a => [a] -> (a, a) -> (a, a)
f [] (c, p) = (c, p)
f (x:xs) (c, p)
    | mod p x == 0 = f xs (c + 1, p)
    | otherwise    = f xs (c, p)

euler39 = f perimeters . (0,) <$> perimeters
