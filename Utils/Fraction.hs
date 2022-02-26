{-

Fractions

Where n is in Z and d in N*


undefined is represented as 0 // 0
+ infinity is represented as  1 // 0
- infinity is represented as -1 // 0
-}
module Utils.Fraction where

import Data.Ratio ( denominator, numerator )

data Fraction = Frac Integer Integer deriving (Eq)

instance Show Fraction where
    showsPrec p (Frac n d)
        | d == 1 = showsPrec p n
        | otherwise = showParen (p > 7) (shows n . showString "/" . shows d)

num :: Fraction -> Integer
num (Frac n d) = n

den :: Fraction -> Integer
den (Frac n d) = d

(//)   :: Integer -> Integer -> Fraction
n // d = reduce $ if d < 0 then Frac (-n) (-d) else Frac n d

infinity :: Fraction
infinity = 1 // 0

undefined :: Fraction
undefined = 0 // 0

instance Ord Fraction where
    compare (Frac n d) (Frac n' d') = compare (n * d') (n' * d)

reduce :: Fraction -> Fraction
reduce (Frac n d) = let g = gcd n d  in Frac (quot n g) (quot d g)

instance Num Fraction where
    (Frac n d) + (Frac n' d') = reduce $ Frac (n * d' + n' * d) (d * d')
    (Frac n d) * (Frac n' d') = reduce $ Frac (n * n') (d * d')
    abs (Frac n d) = Frac (abs n) d
    fromInteger n = Frac n 1
    signum = fromInteger . signum . num
    negate (Frac n d) = Frac (-n) d

instance Fractional Fraction where
    (Frac _ 0) / (Frac _  0 ) = Frac 0 0
    (Frac _ _) / (Frac _  0 ) = Frac 0 1
    (Frac n 0) / (Frac n' _ ) = Frac (signum $ n * n') 0
    (Frac n d) / (Frac n' d') = reduce $ Frac (n * d') (d * n')
    recip (Frac n d)          = if n < 0 then Frac (- d) (- n) else Frac d n
    fromRational a            = Frac n d
                                where n = numerator a
                                      d = denominator a


-- ==========================================================================
--                          Continued Fractions
-- ==========================================================================

-- | Converts a continuous fraction represented as a list
--   to a simplified fraction.
continuousFrac :: [Integer] -> Fraction
continuousFrac []    = infinity
continuousFrac (h:t) = h // 1 + 1 / continuousFrac t

-- | The series of continued fractions for the square root of n
cfSqrt :: Integer -> [Integer]
cfSqrt n = frac' (1, 0, 1) [] []
    where   frac' s acc acc' =
                let (e, s') = step s n in
                if s' `elem` acc'
                then let (h:t) = reverse (e:acc) in h: cycle t
                else frac' s' (e:acc) (s':acc')
            step (a, b, c) n =
                let e = truncate $ (fromIntegral a * sqrt (fromIntegral n) + fromIntegral b) / fromIntegral c in
                let (a', b', c') = (a * c, c^2 * e - b * c, a^2 * n - (b - c * e)^2) in
                let g = gcd a' $ gcd b' c' in
                (e, (quot a' g, quot b' g, quot c' g))
