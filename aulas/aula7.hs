{-Objetivos: introduzir os conceitos de
   a) List Comprehension
   b) Função de alta ordem.
   
   Para tanto, iniciamos com um problema simples e mostramos,
   a cada passo, as possibilidades de melhorias.
   -}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Redundant bracket" #-}
import Data.Char
import System.Win32 (xBUTTON1)
import GHC.Base (TrName)

{- faça f1 capaz de somar uma lista de inteiros se um Char for alfanumérico, 
    ou multiplicar os elementos, caso contrário -}
f1::Char->[Int]->Int
f1 y x
    | isDigit y && x == [] = 0
    | not(isDigit y) && x == [] = 1
    | isDigit y = a + f1 y b
    | otherwise = a * f1 y b
        where (a:b) = x
    
{- reescreva f1 usando casamento de padrão -}
f2::Char->[Int]->Int
f2 c x = f2_aux (isDigit c) x

f2_aux :: Bool -> [Int] -> Int
f2_aux True [] = 0
f2_aux True (a:x) = a + f2_aux True x
f2_aux False [] = 1
f2_aux False (a:x) = a * f2_aux False x

{- reescreva f2 fazendo chamadas de funções para somar ou multiplicar -}
f3::Char->[Int]-> Int
f3 a x
    | isDigit a = soma x
    | otherwise = mult x

soma :: [Int] -> Int
soma [] = 0
soma (x:xs) = x + soma xs

mult :: [Int] -> Int
mult [] = 1
mult (x:xs) = x * mult xs


{- reescreva f3 usando função de alta ordem
   Esta função é didática, pois mostra o uso de função de alta ordem
   Contudo, o booleano não seria necessário se conseguíssemos fazer casamento 
   de padrão com a função parâmetro-}
f4 :: Char -> [Int] -> Int
f4 c [] = 0
f4 c x
    | isDigit c = f4_aux True (+) x
    | otherwise = f4_aux False (*) x

f4_aux :: Bool->(Int->Int->Int)->[Int]->Int
f4_aux b _ [] 
    | b = 0
    |otherwise = 1
f4_aux b op (x:xs) = op x (f4_aux b op xs)


{- 
    Funções de alta ordem
-}
-- Soma e subtrai elementos de uma lista
sumLSubL :: (Int->Int->Int) -> [Int] -> Int
sumLSubL op [] = 0
sumLSubL op (x:xs) = op x (sumLSubL op xs)

-- Altera todos os elementos de uma lista
altera f [] = []
altera f (x:xs) = (f x) : altera f xs

{- funcao que converte caixa baixa para caixa alta
    usar com a funcao map para aplicar a uma string -}
intervalo = ord ('a') - ord ('A')
caixaBaixaAlta c = chr (ord c - intervalo)
