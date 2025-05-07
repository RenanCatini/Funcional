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
mdc :: Int -> Int -> Int
mdc m n
   | m `mod` n /= 0 = mdc n (m `mod` n)
   | otherwise = n

-- Exercicio 9: Função que retorna quantos multiplos de um número existem em um intervalo
howManyMultiples :: Int->Int->Int->Int
howManyMultiples x y z
    | z < y = 0
    | mod z x == 0 = 1 + howManyMultiples x y (z-1)
    | otherwise = howManyMultiples x y (z-1)
 

-- Exercicio 10: Retorna o ultimo digito de um número
lastDigit :: Int->Int
lastDigit x
   | x < 10 = x
   | otherwise = lastDigit (x `mod` 10)
    
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
maiorIdade = maiorIdadeAUX 1 1

maiorIdadeAUX :: Int -> Int -> Int
maiorIdadeAUX x y
    | trc (pessoa x) == 'x' = y
    | sec (pessoa x) > sec (pessoa y) = maiorIdadeAUX (x+1) x
    | otherwise = maiorIdadeAUX (x+1) y


-- Exercicio 32: Recebe 4 inteiros e retorna uma tupla com eles ordenados
ordena4Tupla :: Int -> Int -> Int -> Int -> (Int,Int,Int,Int)
ordena4Tupla w x y z = tupla4 (ordenaLista [w,x,y,z])

tupla4 :: [Int] -> (Int,Int,Int,Int)
tupla4 [w,x,y,z] = (w,x,y,z)

ordenaLista :: [Int] -> [Int]
ordenaLista [] = []
ordenaLista x = ordenaListaAUX x []

ordenaListaAUX :: [Int] -> [Int] -> [Int]
ordenaListaAUX [] y = y
ordenaListaAUX (x:xs) y = ordenaListaAUX xs (insere x y)

insere :: Int -> [Int] -> [Int]
insere x [] = [x]
insere x (y:ys) 
    | x > y = y : insere x ys
    | otherwise = x:(y:ys)

-- Exercicio 33: Quantidade de dias entre datas
qtdDias :: (Int,Int,Int) -> (Int,Int,Int) -> Int
qtdDias (d1,m1,a1) (d2,m2,a2)  
    | m1 >= m2 = (a2 - a1)* 365 + (m2 - m1)*30 + (d2-d1)
    | m1 < m2  = (a2 - a1)* 365 + (m2 - m1)*30 + (d2-d1)

-- Exercicio 34: Resolve Equancao de Segundo grau
equacao :: (Float,Float,Float) -> (Float,Float)
equacao (a,b,c) 
    | (b*b) - (4*a*c) < 0 = error "Raiz negativa"
    | otherwise = (((-b) + sqrt((b*b) - (4*a*c))) / (2*a),
                   ((-b) - sqrt((b*b) - (4*a*c))) / (2*a))

-- Exercicio 35: Triangulo
triangulo :: (Int,Int,Int) -> (String,Int)
triangulo (x,y,z)
    | (x+y < z) || (x+z < y) || (y+z < x) = error "Nao eh triangulo"
    | x == y && y == z = ("Equilatero", x+y+z)
    | x == y || x == z || y == z = ("Isoceles", x+y+z)
    | otherwise = ("Escaleno", x+y+z)


-- Exercicio 36: Base de professores
base :: Int -> (Int, String, String, Char)
base x
    | x == 0  = (1793, "Pedro Paulo", "MESTRE", 'M')
    | x == 1  = (1797, "Joana Silva Alencar", "MESTRE", 'M')
    | x == 2  = (1534, "Joao de Medeiros", "DOUTOR", 'F')
    | x == 3  = (1267, "Claudio Cesar de Sa", "DOUTOR", 'M')
    | x == 4  = (1737, "Paula de Medeiros", "MESTRE", 'F')
    | x == 5  = (1888, "Rita de Matos", "MESTRE", 'F')
    | x == 6  = (1622, "Carlos Henrique Souza", "DOUTOR", 'M')
    | x == 7  = (1777, "Mariana Lopes", "MESTRE", 'F')
    | x == 8  = (1812, "Fernando Almeida", "DOUTOR", 'M')
    | x == 9  = (1698, "Tereza Cristina Andrade", "MESTRE", 'F')
    | x == 10 = (0, "", "", '0')
    | otherwise = (0, "", "", '0')

prm4 :: (a,b,c,d) -> a
prm4 (x,_,_,_) = x

sgd4 :: (a,b,c,d) -> b
sgd4 (_,x,_,_) = x

trc4 :: (a,b,c,d) -> c
trc4 (_,_,x,_) = x

qrt4 :: (a,b,c,d) -> d 
qrt4 (_,_,_,x) = x

-- a) Numero de doutores na base
qtdDoutores :: Int
qtdDoutores = qtdDoutoresAUX 0

qtdDoutoresAUX :: Int -> Int
qtdDoutoresAUX x 
    | prm4 (base x) == 0 = 0
    | trc4 (base x) == "DOUTOR" = 1 + qtdDoutoresAUX (x+1)
    | otherwise = qtdDoutoresAUX (x+1)

-- b) Numero de mulheres
qtdMulheres :: Int
qtdMulheres = qtdMulheresAUX 0

qtdMulheresAUX :: Int -> Int
qtdMulheresAUX x
    | prm4 (base x) == 0 = 0
    | qrt4 (base x) == 'F' = 1 + qtdMulheresAUX (x+1)
    | otherwise = qtdMulheresAUX (x+1)

-- c) Numero de Mestres do sexo Masculino
qtdMestreMasc :: Int
qtdMestreMasc = qtdMestreMascAUX 0

qtdMestreMascAUX :: Int -> Int
qtdMestreMascAUX x 
    | prm4 (base x) == 0 = 0
    | trc4 (base x) == "MESTRE" && qrt4 (base x) == 'M' = 1 + qtdMestreMascAUX (x+1)
    | otherwise =  qtdMestreMascAUX (x+1)

-- d) Nome do professor mais antigo
profMaisAntigo :: String
profMaisAntigo = sgd4 (base (profMaisAntigoAUX 0 0))

profMaisAntigoAUX :: Int -> Int -> Int 
profMaisAntigoAUX x y
    | prm4 (base x) == 0 = y
    | prm4 (base x) < prm4 (base y) = profMaisAntigoAUX (x+1) x
    | otherwise = profMaisAntigoAUX (x+1) y

-- Exercicio 37: 
type Acervo = [(Isbn, Titulo, Reserva, Volumes)]
type Emprestimo = [(Matricula, Isbn)]

type Isbn = Int -- Isbn de um livro
type Volumes = Int -- quantidade no acervo
type Titulo = String -- título do livro
type Matricula = String -- matrícula do discente
type Reserva = Bool -- deve permanecer na biblioteca?

acervo :: Acervo
acervo = [
    (1, "Haskell Básico", False, 3),
    (2, "Programação Funcional", True, 2),
    (3, "Estruturas de Dados", False, 1),
    (4, "Compiladores", False, 0)
    ]

emprestimo :: Emprestimo
emprestimo = [
    ("2023001", 1),
    ("2023002", 1),
    ("2023003", 3)
    ]

-- a) Reservado = True, 
func_1::Isbn-> Acervo -> Bool
func_1 _ [] = error "Isbn nao encontrado"
func_1 x (a:as) 
    | x == prm4 a = not (trc4 a)
    | otherwise = func_1 x as

-- b) quantidade de livros emprestado
func_2::Isbn -> Emprestimo -> Int
func_2 _ [] = 0
func_2 x (a:as)
    | x == (snd a) = 1 + (func_2 x as)
    | otherwise = func_2 x as

-- c) quantidade de livros no acervo
func_3::Isbn -> Acervo -> Int
func_3 _ [] = 0
func_3 x (a:as)
    | x == prm4 a = qrt4 a
    | otherwise = func_3 x as

-- d) quantidade de livros disponiveis para emprestimo
func_4 :: Isbn -> Int
func_4 x
    | (func_3 x acervo) == (func_2 x emprestimo) || not (func_1 x acervo) = 0
    | otherwise = (func_3 x acervo) - (func_2 x emprestimo)

-- e) recebe a matricula e o isbn e ve se pode pegar ou nao o livro
func_5 :: Matricula -> Isbn -> Emprestimo
func_5 mat isbn 
    | (func_4 isbn) /= 0 = (mat,isbn) : emprestimo  
    | otherwise = emprestimo 