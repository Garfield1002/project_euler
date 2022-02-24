permutations :: [a] -> [[a]]
permutations l = permutation' l []
        where permutation' [] e = [[]]
              permutation' [a] e = map (a:) (permutations e)
              permutation' (h:t) e =
                  map (h:) (permutations (e ++ t)) ++ permutation' t (e ++ [h])

euler24 :: [Integer]
euler24 = last $ take 1000000 (permutations [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
