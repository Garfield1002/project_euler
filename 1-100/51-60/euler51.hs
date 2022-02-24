-- The number of digits
n :: Int
n = 6

digits :: [Int]
digits = [-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]

numList :: Int -> [Int]
numList n
    | q == 0    = [r]
    | otherwise = r : numList q
    where (q, r) = quotRem n 10

matches :: (Eq a, Num a) => [a] -> [a] -> Bool
matches (x:xs) []       = (x == 0 || x == -1) && matches xs []
matches (-1:xs) (_:ys)  = matches xs ys
matches (x:xs) (y:ys)   = x == y && matches xs ys
matches _ _ = True

