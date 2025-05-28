import Data.Char (isDigit, isLetter)
import Text.XHtml (rows)
import System.Win32 (COORD(xPos))
-- Generalizacao em Haskell --
{-
    Generalização: Função como argumento, assim gera funcoes de mais alta ordem
-}
mapInt f [] = []
mapInt f (x:xs) = (f x) : mapInt f xs

fold :: (t -> t -> t) -> [t] -> t
fold f [a] = a
fold f (a:x) = f a (fold f x)

sumList l = fold (+) l

filterString :: (Char -> Bool) -> [Char] -> [Char]
filterString p [] = []
filterString p (x:xs)
    | p x = x : filterString p xs
    | otherwise = filterString p xs

digits st = filterString isDigit st
letters st = filterString isLetter st


f :: [Int] -> [Int]
f l = [3*a | a <- l, mod a 2 == 0]

triplo :: Int -> Int
triplo x = 3*x

par x = mod x 2 == 0

lengthS :: [t] -> Int
lengthS [] = 0
lengthS (a:x) = 1 + lengthS x

-- rev :: [t] -> [t]
-- rev [] = []
-- rev l = rev xs ++ [x]
--     where (x:xs) = l

rev :: [t] -> [t]
rev [] = []
rev (x:xs) = rev xs ++ [x]

zipS (x:xs) (y:ys) = (x,y) : zipS xs ys
zipS _ _ = []


-- Materia Online --
{- 01 qual é o resultado da computação desta implementação? -}

f1 :: [(Int,Char)]   
f1 = [ (a,b) | a <-[1..26], b<-['a'..'z']]

{- 02 Considere uma tupla (x, y) que aparece na lista gerada por f1. Faça uma função que remova 
toda ocorrência subsequentes de tuplas (x, _), mantendo apenas a primeira 
    exemplo: f2 [(1,'a'),(1,'b'),(1,'c'),(2,'d'),(2,'e'),(2,'f'),(2,'g')] retorna [(1,'a'),(2,'d')] 
    Observação: você não deve considerar o Char da tupla em sua computação -}
f2 :: [(Int,Char)] -> [(Int,Char)]
f2 l = f2_aux l []

f2_aux :: [(Int,Char)] -> [Int] -> [(Int,Char)]
f2_aux [] _ = []
f2_aux ((n,c):xs) [] = (n,c) : f2_aux xs [n]
f2_aux ((n,c):xs) y 
    | n `elem` y = f2_aux xs y
    | otherwise  = (n,c) : f2_aux xs (n:y)

{-João Pedro-}
-- f2_jp :: [(Int,Char)] -> [Int] -> [(Int,Char)]
-- f2_jp [a] = [a]
-- f2_jp ((i1,c1):(i2,c2):x)
--     | i1 == i2  = f2_jp ((i1,c1):x)
--     | otherwise = (i1,c1) : f2_jp((i2,c2):x)
-- f2_jp [] = []



{- 03 Desafio: considere as repetições das tuplas (x,_) geradas por f1. Então, faça um filtro 
parecido com f2, contudo, para o primeiro Int, a primeira tupla com a ocorrência será mantida na 
solução. Por conseguinte, para o segundo Int, a segunda tupla será mantida, e assim por diante. 
Vejam o exemplo:
f3 [(1,'a'),(1,'b'),(1,'c'),(1,'d'),(2,'a'),(2,'b'),(2,'c'),(2,'d'),(3,'a'),(3,'b'),(3,'c'),
3(3,'d'),(4,'a'),(4,'b'),(4,'c'),(4,'d')] retorna [(1,'a'),(2,'b'),(3,'c'),(4,'d')]
Observação: você não deve considerar o Char da tupla em sua computação -}
f3 :: [(Int,Char)] -> [(Int,Char)]
f3 l = f3_aux l [] 1

f3_aux :: [(Int,Char)] -> [Int] -> Int -> [(Int,Char)]
f3_aux [] _ _ = []
f3_aux ((n,c):xs) [] 1 = (n,c) : f3_aux xs [n] 2
f3_aux ((n,c):xs) y v 
    | n `elem` y = f3_aux xs y v
    | otherwise  = f3_rows ((n,c):xs) v : f3_aux xs (n:y) (v+1) 

f3_rows :: [(Int,Char)] -> Int -> (Int,Char)
f3_rows ((n,c):_) 1 = (n,c)
f3_rows (_:xs) r = f3_rows xs (r-1) 

{-Davi-}
nOcorrencia :: Eq a => Int -> a -> [(a,b)] -> [(a,b)]
nOcorrencia n x ((y,z):ys)
    | n == 1 && x == y = return (y,z)
    | x == y           = nOcorrencia (n-1) x ys
    | otherwise        = nOcorrencia n x ys

f3_auxD :: Eq a => Int -> [a] -> [(a,b)] -> [(a,b)]
f3_auxD _ _ [] = []
f3_auxD n l ((x,c):xs) 
    | x `elem` l = f3_auxD n l xs
    | otherwise  = nOcorrencia n x ((x,c):xs) ++ f3_auxD (n+1) (x:l) xs 

f3_D :: [(Int,Char)] -> [(Int,Char)]
f3_D = f3_auxD 1 [] 


{- 04 Considere a função f4Fold como definida a seguir -}

f4Fold::(t->u->u)->[t]->u->u
f4Fold _ [] z    = z
f4Fold f (a:b) z = f a (f4Fold f b z)
 
{- implemente um exemplo de computação em que t diferente de u para a computação de f4Fold -}
{-Davi-}
f_d :: Char -> Char -> Bool -> Bool
f_d c1 c2 b = c1 == c2 || b









