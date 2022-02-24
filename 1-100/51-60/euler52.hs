
import Data.List

ss :: (Show a) => a -> [Char]
ss = sort . show

condition :: (Num a, Show a) => a -> Bool
condition n =
    s == ss (2 * n) &&
    s == ss (3 * n) &&
    s == ss (4 * n) &&
    s == ss (5 * n) &&
    s == ss (6 * n)
    where s = ss n

f :: Integral a => a -> a
f  n = 10 ^ (ceiling . logBase 10 $ fromIntegral n) + n

l = [f n | n <- [1..], condition . f $ n]

main = print . head $ l
