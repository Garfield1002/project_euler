{-
We want a 16-digit string so 10 is on the outside

Working clockwise, and starting from the group of three with the numerically lowest external node

So we need 6 on the outside -> the exterior ring is composed of [6..10]

The interior ring is [1..5]
-}

import Utils.Miscellaneous ( permutations )


group2 :: [a] -> [[a]]
group2 l = group2' l
  where group2' (h1:h2:t) = [h1, h2] : group2' (h2:t)
        group2' [h]       = [[h, head l]]
        group2' []        = []

isValidRing :: (Eq a, Num a) => [a] -> [a] -> Bool
isValidRing ext int = all (== head ring) (tail ring)
  where ring = zipWith (+) ext (sum <$> group2 int)

ring2Str :: Show a => [a] -> [a] -> [Char]
ring2Str ext = concat . zipWith (\ x y -> concatMap show (x:y)) ext . group2

euler68 :: [Char]
euler68 = maximum [ring2Str e i | e' <- permutations [7..10], let e = 6:e', i <- permutations [1..5], isValidRing e i]
