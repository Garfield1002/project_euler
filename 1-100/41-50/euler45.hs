isect ~(x:xs) ~(y:ys) ~(z:zs)
    | x < y = isect xs (y:ys) (z:zs)
    | y < z = isect (x:xs) ys (z:zs)
    | x == y && y == z = x : isect xs ys zs
    | otherwise = isect (x:xs) (y:ys) zs

t = [n * (n + 1) `quot` 2 | n <- [1..]]
p = [n * (3 * n - 1) `quot` 2 | n <- [1..]]
h = [n * (2* n - 1) | n <- [1..]]

euler45 = take 3 $ isect t p h