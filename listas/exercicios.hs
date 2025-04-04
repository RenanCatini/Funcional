import System.Win32 (xBUTTON1, zeroMemory, aCCESS_SYSTEM_SECURITY)
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

-- Permutação: m!/n!*(m-n)!
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



