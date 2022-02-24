iLength = length . show

main :: IO ()
main = print . length $ [n^x |  n <- [1..9],
                                x <- [1..22],
                                x == iLength (n^x)]
