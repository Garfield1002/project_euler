euler33 =   let l = [ (num, den) |
                    c <- [1..9],
                    d <- [1..9],
                    a <- [1..9],
                    b <- [1..9],
                    let num = a * 10 + b,
                    let den = c * 10 + d,
                    num < den
                    && (den * b == num * d && a == c
                    ||  den * b == num * c && a == d
                    ||  den * a == num * d && b == c
                    ||  den * a == num * c && b == d)
                    ] in
            let num = product $ map fst l in
            let den = product $ map snd l in
            (div num (gcd num den), div den (gcd num den))

