mdc :: Int -> Int -> Int
mdc m n
   | m `mod` n /= 0 = mdc n (m `mod` n)
   | otherwise = n

howManyEqual::Int->Int->Int->Int
howManyEqual x y z
   | (x == y) && (y == z) = 3
   | (x /= y) && (x /= z) && (y /= z) = 0
   | otherwise = 2



