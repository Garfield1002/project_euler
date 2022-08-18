import           Data.Array

generalizedPentagonal :: [Int]
generalizedPentagonal = map pentagonal $ [1 ..] >>= \i -> [i, -i]
  where
    pentagonal n = (3 * n ^ 2 - n) `quot` 2

n :: Int
n = 10 ^ 6

partitions :: Int -> Int
partitions =
  (array (0, n) [(i, partitions' i `mod` (10 ^ 6)) | i <- [0 .. n]] !)
  where
    partitions' 0 = 1
    partitions' n = sum . sign . map partitions . takeWhile (>= 0) . map (n -)
      $ generalizedPentagonal

    sign = zipWith (*) (cycle [1, 1, -1, -1])

sol = fst . head . filter ((== 0) . snd) $ [(i, partitions i) | i <- [1 ..]]