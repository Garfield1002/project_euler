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
