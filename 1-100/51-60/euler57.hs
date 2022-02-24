sq2 :: [[Integer]]
sq2 = sq2' 1 1
    where sq2' n d = [n + d + d, n + d] : sq2' (n + d + d) (n + d)

toDigits :: Integral t => t -> [t]
toDigits n
    | q == 0    = [r]
    | otherwise = r : toDigits q
    where (q, r) = quotRem n 10

f :: Integral a => [a] -> Bool
f ~[n,d] = let [n', d'] = length . toDigits <$> [n,d] in n' > d'

main = print . length . filter f . take 1001 $ sq2