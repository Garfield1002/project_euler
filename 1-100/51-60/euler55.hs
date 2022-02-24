-- Lychrel number

rev :: Integer -> Integer
rev = read . reverse . show

isLychrel :: Integer -> Bool
isLychrel n = isLychrel' (n + rn) 0
    where   isLychrel' n 50  = True
            isLychrel' n i  = n /= rn && isLychrel' (n + rn) (i + 1)
                            where rn = rev n
            rn = rev n

main :: IO ()
main = print . sum $ [1 | n <- [0..10000], isLychrel n]