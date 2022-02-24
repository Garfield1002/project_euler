import Data.List (groupBy, sort)
import Data.Function ( on )

isPermutation :: ([Char], b) -> ([Char], b) -> Bool
isPermutation = (==) `on` fst

main :: IO ()
main =  print
        . minimum
        . map (snd . head)
        . filter ((>= 5) . length)
        . groupBy isPermutation
        . sort
        . map (\ x -> (sort . show $ x, x))
        $ [n^3 | n <- [0..10000]]
