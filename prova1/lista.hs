-- 1) receba dois numeros e retorne a soma dos quadrados deles
somaQuadrados :: Int -> Int -> Int
somaQuadrados x y = (x*x) + (y*y)

-- 2) Recebe um numero e ve se ele eh par
ehPar :: Int -> Bool
ehPar x 
    | x `mod` 2 == 0 = True
    | otherwise = False

-- 3) Maior entre dois numeros
maiorDeDois :: Int -> Int -> Int
maiorDeDois x y
    | x > y = x
    | otherwise = y

-- 4) Area do circulo
areaCirculo :: Float -> Float
areaCirculo x = x*x * 3.1419

-- 5) Retorna o segundo elemento de uma lista
segundoElemento :: [Int] -> Int
segundoElemento [] = -1
segundoElemento [x] = -1
segundoElemento (x:y:yx) = y

-- 6) Retorna o produto de todos os elementos de uma lista
produtoLista :: [Int] -> Int
produtoLista [] = 1
produtoLista (x:xs) = x * produtoLista xs

-- 7) Verifica se todos os valores de uma lista sao pares
todosPares :: [Int] -> Bool
todosPares [] = True
todosPares (x:xs)
    | x `mod` 2 == 0 = todosPares xs
    | otherwise = False

-- 8) Conta quantas vogais uma palavra tem
contaVogais :: String -> Int
contaVogais [] = 0
contaVogais (x:xs)
    | x=='a' || x=='e' || x=='i' || x=='o' || x=='u' = 1 + contaVogais xs
    | x=='A' || x=='E' || x=='I' || x=='O' || x=='U' = 1 + contaVogais xs
    | otherwise = contaVogais xs

-- 9) Cria uma lista com um elemento repetido n vezes
replicaElemento :: Int -> a -> [a]
replicaElemento 0 _ = []
replicaElemento x y = y : replicaElemento (x-1) y

-- 10) Recebe um numero e retorna 2 elevado a ele
potenciaDeDois :: Int -> Int 
potenciaDeDois 0 = 1
potenciaDeDois x = 2 * potenciaDeDois (x-1)

-- 11) fatorial de um número considerando apenas números ímpares
fatorialDuplo :: Int -> Int
fatorialDuplo 0 = 1
fatorialDuplo x 
    | x `mod` 2 /= 0 = x * fatorialDuplo (x-1)
    | otherwise = fatorialDuplo (x-1)

{- 12) dada uma lista de números, calcule a soma de todos os seus prefixos. 
Exemplo: somaPrefixos [1,2,3] = (1) + (1+2) + (1+2+3) = 10. -}
somaPrefixos :: [Int] -> Int 
somaPrefixos x = somaPrefixosAUX x (qtdLista x)

qtdLista :: [Int] -> Int
qtdLista [] = 0
qtdLista (_:xs) = 1 + qtdLista xs

somaPrefixosAUX :: [Int] -> Int -> Int
somaPrefixosAUX [] _ = 0
somaPrefixosAUX (x:xs) y = x*y + somaPrefixosAUX xs (y-1)

{- 13) um número e verifique se ele é perfeito 
(soma dos divisores próprios igual ao próprio número)-}
numeroPerfeito :: Int -> Bool
numeroPerfeito x = (somaLista (divisores x 1)) == x

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

divisores :: Int -> Int -> [Int]
divisores x y 
    | x == y = []
    | x `mod` y == 0 = y : divisores x (y+1)
    | otherwise = divisores x (y+1)

{- 14) duas listas e intercale seus elementos 
(caso uma lista seja maior, ignore os elementos restantes)-}
intercalaListas :: [Int] -> [Int] -> [Int]
intercalaListas [] _ = []
intercalaListas _ [] = []
intercalaListas (x:xs) (y:ys) = x:y:  intercalaListas xs ys

-- 15) Verificar se uma palavra eh palindromo
palindromo :: String -> Bool 
palindromo [] = True
palindromo x = x == (reverseString x)

reverseString :: String -> String 
reverseString [] = []
reverseString (x:xs) = reverseString xs ++ [x]
