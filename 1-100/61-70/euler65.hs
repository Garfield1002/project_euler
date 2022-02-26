import Utils.Fraction ( Fraction, (//), infinity, num, continuousFrac)
import Utils.Miscellaneous (listDigits)

eCF :: [Integer]
eCF = 2 : 1 : eCF' 1
    where eCF' n = (2 * n) : 1 : 1 : eCF' (n + 1)

euler65 :: Integer
euler65 = sum . listDigits . num . continuousFrac $ take 100 eCF
