import Utils.Miscellaneous

fac :: Int -> Int
fac n = product [1 .. n]

chainLength :: Int -> Int
chainLength = length . chain [] . num2digits
  where
    chain :: [[Int]] -> [Int] -> [[Int]]
    chain acc l = if l `elem` acc then acc else chain (l : acc) (num2digits . sum $ map fac l)

main = print . length . filter ((== 60) . chainLength) $ [1 .. 10 ^ 6]
