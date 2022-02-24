import Data.Array

permutations :: [a] -> [[a]]
permutations l = permutation' l []
        where permutation' [] e = [[]]
              permutation' [a] e = map (a:) (permutations e)
              permutation' (h:t) e =
                  map (h:) (permutations (e ++ t)) ++ permutation' t (e ++ [h])

isGood :: Integral a => [a] -> Bool
isGood d =
            even (d !! 3)
            && rem (d !! 2 + d !! 3 + d !! 4) 3 == 0
            && rem (d !! 5) 5 == 0
            && rem (10 * d !! 4 + d !! 5 - 2 * d !! 6) 7 == 0
            && rem (d !! 5 + d !! 7 - d !! 6) 11 == 0
            && rem (100 * d !! 6 + 10 * d !! 7 + d !! 8) 13 == 0
            && rem (100 * d !! 7 + 10 * d !! 8 + d !! 9) 17 == 0

intList :: Num p => [p] -> p
intList = intList' . reverse
    where   intList' [] = 0
            intList' (h:t) = h + 10 * intList' t

euler43 :: Integer
euler43 = sum . map intList . filter isGood . permutations $ [0..9]
