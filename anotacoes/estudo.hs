import Data.Char

-- Alguns exercicios para estudar Funcional

allEqual :: Int -> Int -> Int -> Bool
allEqual x y z = (x==y) && (x==z)

maxi :: Int -> Int -> Int
maxi x y
    | x > y = x
    | otherwise = y

periodo::Int
periodo = 7

f :: Int -> Int
f 1 = 9
f 2 = 7
f 3 = 15
f 4 = 14
f 5 = 8
f 6 = 1
f 7 = 3
f x = -1

maiorVenda :: Int -> Int -> Int
maiorVenda 0 y = y
maiorVenda x y
    | f x > y = maiorVenda (x-1) (f x)
    | otherwise = maiorVenda (x-1) y

diaMaiorVenda :: Int -> Int -> Int
diaMaiorVenda 0 y = y
diaMaiorVenda x y
    | f x > f y = diaMaiorVenda (x-1) x
    | otherwise = diaMaiorVenda (x-1) y

totalVendas :: Int -> Int
totalVendas x
    | x == (periodo+1) = 0
    | otherwise = f x + totalVendas (x+1)

mediaVendas :: Int -> Int
mediaVendas 0 = 0
mediaVendas x = (totalVendas 1) `div` x

maiorVenda01 :: Int -> Int
maiorVenda01 0 = 0
maiorVenda01 x = maxi (maiorVenda01 (x-1)) (f x)

somaLista :: [Int] -> Int
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

searchList :: Int -> [Int] -> Bool
searchList _ [] = False
searchList x (y:ys) 
    | x == y = True
    | otherwise = searchList x ys

deleteList :: Int -> [Int] -> [Int]
deleteList _ [] = []
deleteList x (y:ys)
    | y == x = deleteList x ys
    | otherwise = y : deleteList x ys

lenghtList :: [Int] -> Int
lenghtList [] = 0
lenghtList (_:xs) = 1 + lenghtList xs

contList :: Int -> [Int] -> Int
contList _ [] = 0
contList x (y:ys) 
    | x == y = 1 + contList x ys
    | otherwise = contList x ys

invertList :: [Int] -> [Int]
invertList [] = []
invertList (x:xs) = invertList xs ++ [x]

listaVendas :: Int -> [Int]
listaVendas 0 = []
listaVendas x = listaVendas (x-1) ++ [f x]

listaDiaVendas :: Int -> [[Int]]
listaDiaVendas 0 = []
listaDiaVendas x = listaDiaVendas (x-1) ++ [[x,f x]]

ordenaLista :: [Int] -> [Int]
ordenaLista [] = []
ordenaLista (x:xs) = insere x (ordenaLista xs)

insere :: Int -> [Int] -> [Int]
insere x [] = [x]
insere x (y:ys)
    | x > y = y : insere x ys
    | otherwise = x:y:ys

myUnzip :: [(Int,Char)]->([Int],[Char])
myUnzip [] = ([],[])
myUnzip ((int,char):xs) = (int : fst (myUnzip xs), char : snd (myUnzip xs))

myZip::[Bool]->[Char] ->[(Bool,Char)]
myZip [] [] = []
myZip (b:bs) (c:cs) = (b,c) : myZip bs cs 

setAlfa::String -> [(Bool,Char)]
setAlfa [] = []
setAlfa (a:as)
    | isAlphaNum a = (True,a) : setAlfa as
    | otherwise = (False,a) : setAlfa as

filtraAlfa:: [(Bool,Char)] -> String
filtraAlfa [] = []
filtraAlfa ((bool,char):xs)
    | bool = char : filtraAlfa xs
    | otherwise = filtraAlfa xs

alfaToInt::String -> [Int]
alfaToInt [] = []
alfaToInt (x:xs) = (ord x) : (alfaToInt xs)

-- Funcao f1
repete :: Char -> Int -> String
repete _ 0 = []
repete x y = x : repete x (y-1) 

charToInt :: Char -> Int
charToInt x = ord x - ord '0'

f1 :: String -> String
f1 [] = []
f1 (x:y:xs)
    | isDigit x && not (isDigit y) = (x : (repete y (charToInt x))) ++ f1 xs 
    | otherwise = x : (f1 (y:xs))

--


