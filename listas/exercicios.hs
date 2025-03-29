import System.Win32 (xBUTTON1)
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
mult 1 y = y
mult x y = soma y (mult (x-1) y)

-- Exercício 4: Inverter valor
     
