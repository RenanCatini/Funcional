import Data.Char

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

antiFib :: Int -> Int
antiFib x = antiFibAUX x 0

antiFibAUX :: Int -> Int -> Int
antiFibAUX x y
    | x == fib y = y
    | x > fib y = antiFibAUX x (y+1)
    | x < fib y = -1


-- Exercicio 16: 
funny :: Int -> Int -> Int -> Bool
funny x y z
    | x > z = True
    | y >= x = False
    | otherwise = True

funny2 :: Int -> Int -> Int -> Bool
funny2 x y z = (x > z) || (x > y) 


-- Exercicio 17: Converte de letra minuscula para maiuscula
paraMaiuscula :: Char -> Char
paraMaiuscula x
  | ord x >= ord 'a' && ord x <= ord 'z' = chr (ord x - (ord 'a' - ord 'A'))
  | otherwise = x

-- Exercicio 18: Converte char para int
charToNum::Char->Int
charToNum x 
    | isDigit x = ord x - ord '0'
    | otherwise = -1  

-- Exercicio 19: Concatenar n vezes a string
duplicate :: String -> Int -> String 
duplicate _ 0 = ['.']
duplicate s 1 = s
duplicate s n = s ++ duplicate s (n-1)

-- Exercicio 20: 
tamString :: String -> Int
tamString [] = 0
tamString (_:xs) = 1 + tamString xs

pushRight :: String -> Int-> String 
pushRight s n 
    | n > tamString s = ">" ++ pushRight s (n-1)
    | otherwise = s

-- Exercicio 21: ?????

-- Exercicio 22: Inverter lista
inverte :: [Int] -> [Int]
inverte [] = []
inverte (x:xs) = inverte xs ++ [x]

-- Exercici 23: Separar uma lista em uma de pares e de impares
separa :: [Int] -> ([Int],[Int])
separa [] = ([],[])
separa (x:xs)
    | x `mod` 2 == 0 = (fst (separa xs), x : snd (separa xs))
    | otherwise = (x : fst (separa xs), snd (separa xs))

-- Exercicio 24: Converte de numero para a letra referendte a posicao no alfabeto
converte :: [Int] -> String
converte [] = ""
converte (x:xs) = chr (x + ord 'A' - 1) : converte xs

-- Exercicio 25: ??????????

-- Exercicio 26: conta quantos de um determinado char existem na String
conta :: String -> Char -> Int
conta [] _ = 0
conta (x:xs) y
    | x == y = 1 + conta xs y
    | otherwise = conta xs y

-- Exercicio 27: retira elementos repetidos da lista
purifica :: [Int] -> [Int]
purifica [] = []
purifica (x:xs)
    | pertence x xs = purifica xs
    | otherwise = x : purifica xs

pertence :: Int -> [Int] ->  Bool
pertence _ [] = False
pertence y (x:xs)
    | x == y = True
    | otherwise = pertence y xs 

-- Exercicio 28: Predicado que faz uma lista de int que cada int passado sera reproduzzido sua vez
proliferaInt :: [Int] -> [Int]
proliferaInt [] = []
proliferaInt (x:xs) = addNVez x x ++ proliferaInt xs

addNVez :: Int -> a -> [a]
addNVez 0 _ = []
addNVez x y = y : addNVez (x-1) y

-- Exercicio 29: Mesmo do que o de cima mas agora com char
charToInt :: Char -> Int
charToInt x = ord x - ord 'A' + 1

proliferaChar :: [Char] -> [Char]
proliferaChar [] = []
proliferaChar (x:xs) = addNVez (charToInt x) x ++ proliferaChar xs

-- Exercicio 30: tripla maiusculo ou minusculi e sua posicao na tabela ascii
converte2 :: Char -> (Char,Char,Int)
converte2 x
    | ord x > (ord 'a' - 1) = (x, toUpper x, ord x)
    | otherwise = (toLower x, x, ord x)

-- Exercicio 31: Funcoes de rg
pessoa :: Int -> (String, Int, Char)
pessoa rg
    | rg == 1 = ("Joao Silva", 12, 'm')
    | rg == 2 = ("Jonas Souza", 51, 'm')
    | rg == 3 = ("Maria Oliveira", 34, 'f')
    | rg == 4 = ("Ana Costa", 28, 'f')
    | rg == 5 = ("Carlos Pereira", 45, 'm')
    | rg == 6 = ("Fernanda Lima", 19, 'f')
    | rg == 7 = ("Paulo Santos", 60, 'm')
    | rg == 8 = ("Juliana Alves", 25, 'f')
    | rg == 9 = ("Ricardo Mendes", 33, 'm')
    | rg == 10 = ("Beatriz Rocha", 40, 'f')
    | otherwise = ("Não há ninguém mais", 9999, 'x')

prm :: (a,b,c) -> a
prm (x,_,_) = x

sec :: (a,b,c) -> b
sec (_,x,_) = x

trc :: (a,b,c) -> c
trc (_,_,x) = x

-- a) pessoa de menor de idade ate determinado registro
menorIdade :: Int -> String
menorIdade x = menorIdadeAUX x x

menorIdadeAUX :: Int -> Int -> String
menorIdadeAUX 0 x = prm (pessoa x)
menorIdadeAUX n x
    | sec (pessoa n) < sec (pessoa x) = menorIdadeAUX (n-1) n
    | otherwise = menorIdadeAUX (n-1) x

-- b) Idade media das pessoas ate determinado registro
idadeMedia :: Int -> Int
idadeMedia 0 = 0
idadeMedia x = (somaIdades x) `div` x

somaIdades :: Int -> Int
somaIdades 0 = 0
somaIdades x = sec (pessoa x) + somaIdades (x-1)

-- c) numero de pessoas do sexo masculino
numDeMasc :: Int
numDeMasc = numDeMascAUX 1

numDeMascAUX :: Int -> Int
numDeMascAUX n 
    | trc (pessoa n) == 'm' = 1 + numDeMascAUX (n+1)
    | trc (pessoa n) == 'f' = numDeMascAUX (n+1)
    | trc (pessoa n) == 'x' = 0
    | otherwise = numDeMascAUX (n+1)

-- d) Numero do registro da pessoa de maior idade
maiorIdade :: Int

maiorIdadeAUX :: Int -> Int -> Int
maiorIdadeAUX 1 = 1
maiorIdadeAUX x
    | 