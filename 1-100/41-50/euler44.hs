import GHC.Int (neInt32)
p :: Integral a => a -> a
p n = quot (n * (3 * n - 1))  2

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

isPentagonal :: Integral t => t -> Bool
isPentagonal x = isInt $ (sqrt (24.0 * fromIntegral x + 1.0) + 1.0) / 6.0

l = [p1 - p2 |
   n1 <- [2 .. ],
   let p1 = p n1,
   n2 <- [1 .. (n1 - 1)],
   let p2 = p n2,
   isPentagonal (p1 + p2) && isPentagonal( p1 - p2)]

mul :: Num a => a -> a -> a
mul x  y = x*y

main = print . head $  l
