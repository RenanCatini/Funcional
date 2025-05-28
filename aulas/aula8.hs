-- List Compehencion

f1 :: Int -> [Int] -> [Int]
f1 x l = [x*a | a <- l]


f2 :: Int -> [Int] -> [Int]
f2 x l = [a*x | a <- l, (mod a 2 == 0)]


f3 :: Int -> [Int] -> [Int]
f3 x l = [x*a | a <- l, (mod a 2 == 0)] ++ [a | a <- l, (mod a 2 /= 0)]



