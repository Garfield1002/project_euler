import Utils.Fraction ( Fraction, (//), infinity, num)
import Utils.Miscellaneous (listDigits)

{-
    Converts a continuous fraction represented as a list
    to a tuple representing a simplified fraction.
-}
continuousFrac :: [Integer] -> Fraction
continuousFrac []    = infinity
continuousFrac (h:t) = h // 1 + 1 / continuousFrac t

eCF :: [Integer]
eCF = 2 : 1 : eCF' 1
    where eCF' n = (2 * n) : 1 : 1 : eCF' (n + 1)

euler65 :: Integer
euler65 = sum . listDigits . num . continuousFrac $ take 100 eCF
