import qualified Data.Array as A
import           Data.Array ((!))
import           Utils.Miscellaneous (combinations)
import qualified Data.Bifunctor
import qualified Data.List as L
import           Data.Ord (comparing, Down(Down))
import           Data.Function (on)

diceProba :: Int -> Double
diceProba p = 1 / fromIntegral (s ^ n)
  * (fromIntegral . sum
     $ [(-1) ^ k
         * combinations n k
         * combinations (p - s * k - 1) (p - s * k - n)
       | k <- [0 .. kMax]])
  where
    n = 2 -- The number of dice

    s = 4 -- The number of faces

    kMax = quot (p - n) s

-- Monopoly single roll probability
msp :: [(Double, Int)]
msp = [( diceProba x
         - if even x
           then 1 / 36
           else 0
       , x)
      | x <- [3 .. 11]]

-- Monopoly doubles roll probability
mdp :: [(Double, Int)]
mdp = [(1 / 36, x) | x <- [2, 4 .. 12]]

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thr3 :: (a, b, c) -> c
thr3 (_, _, x) = x

sum3 :: Num a => (a, a, a) -> a
sum3 (a, b, c) = a + b + c

addI :: Num c => c -> (c, c, c) -> (c, c, c)
addI a (x, y, z) = (x + a, y + a, z + a)

mulI :: Num c => c -> (c, c, c) -> (c, c, c)
mulI a (x, y, z) = (x * a, y * a, z * a)

add3 :: (Num a, Num b, Num c) => (a, b, c) -> (a, b, c) -> (a, b, c)
add3 (a, b, c) (x, y, z) = (a + x, b + y, c + z)

euler84 n = foldl (\acc e -> 100 * acc + e) 0
  . map fst
  . take 3
  . L.sortBy (comparing Down `on` snd)
  . map (Data.Bifunctor.bimap fst fst3)
  . filter ((== n) . snd . fst)
  . A.assocs
  $ prob
  where
    go x y
      | y == 0 = (1 / 40, (1 / 6) * (1 / 40), (1 / 36) * (1 / 40))
      | x == 30 = (0.0, 0.0, 0.0)
      | x == 10 = addI ((1 / 6) ^ 3) -- rolling 3 doubles
        . add3 (standard 30) -- go to jail
        . add3 (mulI (1 / 16) $ standard 2) -- CC1
        . add3 (mulI (1 / 16) $ standard 17) -- CC2
        . add3 (mulI (1 / 16) $ standard 33) -- CC3
        . add3 (mulI (1 / 16) $ standard 7) -- CH1
        . add3 (mulI (1 / 16) $ standard 22) -- CH2
        . add3 (mulI (1 / 16) $ standard 36) -- CH3
        $ standard 10
      | x == 0 = add3 (mulI (1 / 16) $ standard 2) -- CC1
        . add3 (mulI (1 / 16) $ standard 17) -- CC2
        . add3 (mulI (1 / 16) $ standard 33) -- CC3
        . add3 (mulI (1 / 16) $ standard 7) -- CH1
        . add3 (mulI (1 / 16) $ standard 22) -- CH2
        . add3 (mulI (1 / 16) $ standard 36) -- CH3
        $ standard 0
      | x == 11 = add3 (mulI (1 / 16) $ standard 7) -- CH1
        . add3 (mulI (1 / 16) $ standard 22) -- CH2
        . add3 (mulI (1 / 16) $ standard 36) -- CH3
        $ standard 11
      | x == 24 = add3 (mulI (1 / 16) $ standard 7) -- CH1
        . add3 (mulI (1 / 16) $ standard 22) -- CH2
        . add3 (mulI (1 / 16) $ standard 36) -- CH3
        $ standard 24
      | x == 39 = add3 (mulI (1 / 16) $ standard 7) -- CH1
        . add3 (mulI (1 / 16) $ standard 22) -- CH2
        . add3 (mulI (1 / 16) $ standard 36) -- CH3
        $ standard 39
      | x == 5 = add3 (mulI (1 / 16) $ standard 7) -- CH1
        . add3 (mulI (1 / 16) $ standard 22) -- CH2
        . add3 (mulI (3 / 16) $ standard 36) -- CH3
        $ standard 5
      | x == 15 = add3 (mulI (1 / 8) $ standard 7) -- CH1
        $ standard 15
      | x == 25 = add3 (mulI (1 / 8) $ standard 22) -- CH2
        $ standard 25
      | x == 12 = add3 (mulI (1 / 16) $ standard 7) -- CH1
        . add3 (mulI (3 / 16) $ standard 36) -- CH3
        $ standard 12
      | x == 28 = add3 (mulI (1 / 16) $ standard 22) -- CH1
        $ standard 28
      | x == 28 = add3 (mulI (1 / 16) $ standard 22) -- CH1
        $ standard 28
      | x == 19 = add3 (mulI (1 / 16) $ standard 22) -- CH2
        $ standard 19
      | x == 33 = add3 (mulI (1 / 16) $ standard 36) -- CH3
        . mulI (7 / 8)
        $ standard 33
      | x == 17 = mulI (7 / 8) $ standard 17
      | x == 2 = mulI (7 / 8) $ standard 2
      | x == 7 = mulI (3 / 8) $ standard 7
      | x == 22 = mulI (3 / 8) $ standard 22
      | x == 36 = mulI (3 / 8) $ standard 36
      | otherwise = standard x
      where
        end x = sum
          $ map (\(p, dx) -> p * sum3 (prob ! (mod (x - dx) 40, y - 1))) msp

        double x = sum
          $ map (\(p, dx) -> p * fst3 (prob ! (mod (x - dx) 40, y - 1))) mdp

        ddouble x = sum
          $ map (\(p, dx) -> p * snd3 (prob ! (mod (x - dx) 40, y - 1))) mdp

        standard x = (end x, double x, ddouble x)

    prob =
      A.listArray ((0, 0), (39, n)) [go x y | x <- [0 .. 39], y <- [0 .. n]]
