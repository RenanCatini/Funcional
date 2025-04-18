{- objetivo: trabalhar tipos distintos entre listas e tuplas -}
import Data.Char

------------------------------------------------------
{- 01 função que separa [(Int,Char)] em ([Int],[Char]) -}
myUnzip :: [(Int,Char)]->([Int],[Char])
myUnzip [] = ([],[])
myUnzip ((int, char):resto) = (int : fst (myUnzip resto), char : snd (myUnzip resto))

{- 02 versão em uma única função -}
myUnzipU :: [(Int,Char)]->([Int],[Char])
myUnzipU [] = ([],[])
myUnzipU ((int,char):resto) =
   let (ints,chars) = myUnzipU resto 
   in (int : ints, char : chars)

------------------------------------------------------------
{- 03 função que junta duas listas em lista de duplas -}
myZip::[Bool]->[Char] ->[(Bool,Char)]  
myZip [] [] = []
myZip (bool:resto1) (char:resto2) = (bool, char) :myZip resto1 resto2


{- 04 função que recebe [Char] e retorna [(Bool,Char)] 
   True se Char for alfanumérico e False, caso contrário -}
setAlfa::String -> [(Bool,Char)] 
setAlfa [] = []
setAlfa (x:xs) 
   | isAlphaNum x = (True, x) : setAlfa xs
   | otherwise = (False, x) : setAlfa xs


{- 05 função que recebe [(Bool, Char)] e filtra alfanuméricos -}
filtraAlfa:: [(Bool,Char)] -> String
filtraAlfa [] = []
filtraAlfa ((bool,char):resto)
   | bool = char : filtraAlfa resto 
   | otherwise = filtraAlfa resto

{- 06 função transforma String de alfa em Int -}
alfaToInt::String -> [Int]
alfaToInt [] = []
alfaToInt (x:xs) = ord x : alfaToInt xs

{-- 07 função que gera tabela ascii -}
geraASCII::Int->[(Int,Char)]
geraASCII 128 = []
geraASCII x = (x, chr x) : geraASCII (x+1)