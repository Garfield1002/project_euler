import Data.List ( nub )
import Control.Parallel.Strategies ( parList, rdeepseq, using )

listToInt :: [Int] -> Int
listToInt = listToInt' . reverse
    where listToInt' [] = 0
          listToInt' (h:t) = h + 10 *  listToInt' t

threePartition :: [a] -> [[[a]]]
threePartition l = threePartition' l [] [] []
    where threePartition' [] [] _ _   = []
          threePartition' [] _ [] _   = []
          threePartition' [] _ _ []   = []
          threePartition' [] a b c    = [[a, b, c]]
          threePartition' (h:t) a b c = threePartition' t (h:a) b c
                                        ++ threePartition' t a (h:b) c
                                        ++ threePartition' t a b (h:c)

-- f :: [[Int]] -> Bool
-- f [] = False
-- f (h:t) = listToInt h == product (map listToInt t)

permutations :: [Int] -> [[Int]]
permutations l = permutation' l []
        where permutation' [] e = [[]]
              permutation' [a] e = map (a:) (permutations e)
              permutation' (h:t) e =
                  map (h:) (permutations (e ++ t)) ++ permutation' t (h:e)

validEquation ::[Int] ->  Bool
validEquation l = head l /= -1 && validEquation' l
    where validEquation' []      = True
          validEquation' [-1]    = False
          validEquation' [_]     = True
          validEquation' [-1, _] = False
          validEquation' [-1, _, _]    = False
          validEquation' (-1:1:(-1):q) = False
          validEquation' (-1:5:(-1):q) = False
          validEquation' (a:b:q)       = a /= b && validEquation' (b:q)

toEq :: [Int] -> [Int]
toEq = toEq' 0 0 0 0
    where toEq' s a b c []     = [a,b,c]
          toEq' s a b c (-1:t) = toEq' (s + 1) a b c t
          toEq' 0 a b c (h:t)  = toEq' 0 (10 * a + h) b c t
          toEq' 1 a b c (h:t)  = toEq' 1 a (10 * b + h) c t
          toEq' _ a b c (h:t)  = toEq' 2 a b (10 * c + h) t

validElem :: (Eq a, Num a) => [a] -> Bool
validElem [a, b, c] = a * b == c
validElem _ = False


euler32 :: Int
euler32 = sum $ nub [last l|
    l <- map toEq $ filter validEquation (permutations ((-1):(-1):[1..9])),
    validElem l
    ]