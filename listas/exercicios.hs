import System.Win32 (xBUTTON1, zeroMemory, aCCESS_SYSTEM_SECURITY, SECURITY_ATTRIBUTES (bInheritHandle))
-- Exercicio 1 

-- a)
f1 :: Float -> Float
f1 x
    | x >= 0 = (x+4)/(x+2)
    |otherwise = 2/x

f2 :: Float -> Float -> Float
f2 x y 
    | x >= y = x + y
    |otherwise = x - y

f3 :: Float -> Float -> Float -> Float
f3 x y z
    |(x + y) > z = x + y + z
    |(x + y) < z = x - y - z
    |otherwise = 0


-- Exercício 2: Encontre o erro
--  fat::Int->Int
--  fat x = x * fat(x-1)

fat :: Int -> Int
fat 0 = 1   --Condição de parada
fat x = x * fat(x-1)

-- Exercício 3:
--  Considere a função em Haskell soma::Int->Int->Int que retorna a soma 
-- entre os dois parâmetros. Assim, faça uma função em Haskell que resulte 
-- a multiplicação de dois parâmetros fazendo uso da função soma.

soma :: Int -> Int -> Int
soma x y = x + y

mult :: Int -> Int -> Int
mult x y 
    | x == 0 || y == 0  = 0
    | x > 0 && y > 0    = soma y (mult (x-1) y)
    | x < 0             = negate(mult (-x) y)
    | otherwise         = negate(mult x (-y))

-- Exercício 4: Inverter valor
     
-- Exercício 5: Quarta Potencia
square :: Int->Int
square x = x * x

fourPower :: Int->Int
fourPower x = square (square x)

-- Exercicio 6: i-esimo elemento de sqrt 6
seq6 :: Int->Double
seq6 0 = sqrt 6
seq6 x = sqrt (6 + seq6(x - 1))

-- Exercicio 7: Permutação: m!/n!*(m-n)!
fatD :: Double->Double
fatD 0 = 1
fatD x = x * fatD(x-1)

escolha :: Double->Double->Double
escolha m n = fatD m / (fatD n * fatD (m-n))


-- Exercicio 8: MDC
mdc :: Int->Int->Int
mdc x 0 = x
mdc x y = mdc y (mod x y)

-- Exercicio 9: Função que retorna quantos multiplos de um número existem em um intervalo
howManyMultiples :: Int->Int->Int->Int
howManyMultiples x y z
    | z < y = 0
    | mod z x == 0 = 1 + howManyMultiples x y (z-1)
    | otherwise = howManyMultiples x y (z-1)
 

-- Exercicio 10: Retorna o ultimo digito de um número
lastElmList :: [a]->[a]
lastElmList [x] = [x]
lastElmList (_:y) = lastElmList y

lastDigit :: Int->Int
lastDigit x = read (lastElmList (show x))
    
-- Exercicio 11: Retorna o numero que esta na posição informada
anyDigitAux :: Int->[a]->[a]
anyDigitAux 0 (x:_) = [x]
anyDigitAux x (a:b) = anyDigitAux (x-1) b

anyDigit :: Int->Int->Int
anyDigit x y = read (anyDigitAux x (show y))

-- Exercicio 12: 3 números inteiros se são diferentes
allDifferent :: Int->Int->Int->Bool
allDifferent m n p = (m /= n) && (n /= p) && (m /= p)

-- Exercicio 13: Quantos números são iguais?
howManyEqual :: Int->Int->Int->Int
howManyEqual x y z
    | (x /= y) && (x /= z) && (y /= z) = 0
    | (x == y) && (x == z) && (y == z) = 3
    | otherwise = 2

-- Exercicio 14:
periodo::Int
periodo = 7

sales :: Int->Int
sales 1 = 41
sales 2 = 72
sales 3 = 48
sales 4 = 2
sales 5 = 91
sales 6 = 55
sales 7 = 30
sales _ = -1

-- a) howManyLess valor comeco fim retorno
howManyLess :: Int->Int->Int->Int
howManyLess x a b
    | a > b || sales a == -1 = 0
    | sales a < x = 1 + howManyLess x (a+1) b 
    | otherwise = howManyLess x (a+1) b

-- b) Se há zeros no periodo
noZerosInPeriod :: Int->Bool
noZerosInPeriod a 
    | sales a == -1 = True
    | sales a == 0 = False 
    | otherwise = noZerosInPeriod (a+1)


-- c) Quantidade de vendas iguais a zero
zeroInPeriod :: [Int]
zeroInPeriod = zerosPeriodToList 1

zerosPeriodToList :: Int->[Int]
zerosPeriodToList a
    | sales a == -1 = []
    | sales a == 0 = zerosPeriodToList (a+1) ++ [a]
    | otherwise = zerosPeriodToList (a+1)

-- d) Retorna lista de dias que a venda foi menor que um parametro
daysLassThan :: Int->[Int]
daysLassThan a = daysLassThanToList a 1

daysLassThanToList :: Int->Int->[Int]
daysLassThanToList x a 
    | sales a == -1 = []
    | sales a < x = daysLassThanToList x (a+1) ++ [a]
    | otherwise = daysLassThanToList x (a+1)


-- Exercicio 15: Anti Fibonati
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

-- antFib :: Int->Int
-- antFib 0 = 0
-- antFib 1 = 1
-- antFib x =
--     | x >  





