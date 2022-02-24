import Control.Parallel.Strategies ( parList, rdeepseq, using )

collatz :: Int -> [Int]
collatz n = collatz'
    where collatz' = n : map (\n -> if even n then div n 2 else 3 * n + 1) collatz'

collen :: [Int] -> Int
collen [] = 0
collen (h:t) = if h == 1 then 1 else 1 + collen t

euler14 :: Int
euler14 = snd $ maximum l
    where l = [(collen $ collatz n, n) | n <- [1..1000000]] `using` parList rdeepseq

main = print euler14

-- add some dynamic programming elements
