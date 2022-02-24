import Control.Parallel.Strategies ( parList, rdeepseq, using )

listDigits :: Integral t => t -> [t]
listDigits n
    | q == 0    = [r]
    | otherwise = r:listDigits q
    where (q, r) = divMod n 10

fac :: (Eq p, Num p) => p -> p
fac n
    | n == 0    = 1
    | n == 1    = 1
    | n == 2    = 2
    | n == 3    = 6
    | n == 4    = 24
    | n == 5    = 120
    | n == 6    = 720
    | n == 7    = 5040
    | n == 8    = 40320
    | n == 9    = 362880
    | otherwise = n * fac (n - 1)

euler34 :: Integer
euler34 =   sum l
            where l = [ n |  n <- [10..2250000], n == sum ( map fac $ listDigits n)] `using` parList rdeepseq

