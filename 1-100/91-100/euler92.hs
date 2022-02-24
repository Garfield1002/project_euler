import Data.List (groupBy, sort)
import Util (mapFst)
import Data.Bifunctor (first)
import Data.Function (on)

-- Count type ?

toChain :: Integer -> Integer
toChain 1   = 1
toChain 89  = 89
toChain n   = sum . map ((^2) . read . (:[])) $ show n

euler92 :: [(Integer, Integer)] -> Integer
euler92 [(1, _), (89, r)] = r
euler92 l = euler92
            . map (\ l -> (fst . head $ l, sum . map snd $ l))
            . groupBy ((==) `on` fst)
            . sort
            . map (first toChain)
            $ l

main :: IO ()
main = print . euler92 $ [(n, 1) | n <- [1..10^7]]
