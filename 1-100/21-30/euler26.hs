toList :: Integral a => a -> [a]
toList = reverse . toList'
    where toList' i
            | q == 0    = [r]
            | otherwise = r : toList' q
            where (q,r) = divMod i 10

toFrac ::Integral a => a -> [a]
toFrac n = toList $ mod (div (10^3000) n) (10^2000)

equalsUpTo :: Eq a => [a] -> [a] -> Bool
equalsUpTo _ [] = True
equalsUpTo [] _ = True
equalsUpTo (h:t) (h':t') = h == h' && equalsUpTo t t'

cycleLen :: Eq a => [a] -> Int
cycleLen l = cycleLen' l []
        where cycleLen' [] _ = 0
              cycleLen' (h:t) p = let v = p ++ [h] in
                                  if equalsUpTo (cycle v) l
                                  then length p + 1
                                  else cycleLen' t v

f :: Integer -> Int
f = cycleLen . toFrac

euler26 = [(v , n) | n <- [800..1000], let v = f n , v > 500]