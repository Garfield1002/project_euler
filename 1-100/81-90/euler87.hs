import Utils.Primes
import Data.List ( group, sort )

limit = 50 * 10^6

-- `group . sort` has a complexity of `O(N log N)`
-- it is therefore considerably quicker than nub which has a complexity of `O(N^2)`
euler87 = length . group . sort $ [c |
    a <- takeWhile (< limit) . map (^4)             $ primes,
    b <- takeWhile (< limit) . map ((+ a)  . (^3))  $ primes,
    c <- takeWhile (< limit) . map ((+ b)  . (^2))  $ primes
    ]
