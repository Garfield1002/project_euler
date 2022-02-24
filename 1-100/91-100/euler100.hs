import Data.Number.CReal

dico :: Integer -> Integer
dico n = dico' (div n 2) n (div n 2)
    where dico' l u b
            | 2 * (b^2 - b) == n^2- n           = b
            | u - l <= 1                            = -1
            | div (2 * (b^2 - b)) (n^2- n) == 0     = dico' b u (div (b+u) 2)
            | otherwise                             = dico' l b (div (l+b) 2)


f :: Integral a2 => a2 -> CReal
f b = (1 + sqrt (fromIntegral (1 + 8*(b*(b-1))))) / 2

isInt :: CReal -> Bool
isInt x = x == fromInteger (round x)

demi = 2 * 707106788769 * (707106788769 - 1) == 1000000010723 * (1000000010723 - 1)

euler100 = [(b, n) |
   b <- [707106780000..],
   let n = f b,
   let t = floor n,
   fromIntegral (2*b*(b-1)) == t*(t-1)]

main = print $ head euler100
