-- Should be reversed
decToBin :: Int -> [Int]
decToBin 0 = []
decToBin y = let (a,b) = quotRem y 2 in b : decToBin a

isPalindrome :: Int -> Bool
isPalindrome n = b == reverse b where b = decToBin n

pal1 :: Int -> Int
pal1 n = read $ s ++ reverse s where s = show n

pal2 :: Int -> Int
pal2 n = let s = show n in read $ reverse (tail s) ++ s

euler36 n = [p1 | n <- [1..n],
                    let p1 = pal1 n,
                    isPalindrome p1]
         ++ [p2 | n <- [1..n],
                    let p2 = pal2 n,
                    isPalindrome p2]
