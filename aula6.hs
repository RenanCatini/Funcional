import Data.Char

{-01 função que retorna lista de duplas com char e posição na ASCII -}
listaDuplaCharInt:: Int-> [(Char,Int)]
listaDuplaCharInt 0 = []
listaDuplaCharInt x = listaDuplaCharInt (x-1) ++ [(chr x, x)]

{-02 função meuChar que pesquisa um char pelo int na lista gerada -}
meuChar :: Int -> [(Char,Int)] -> Char
meuChar _ [] = error "Valor nao encontrado na lista"
meuChar x ((c,i):ys)
    | x == i = c
    | otherwise = meuChar x ys

{-03 função meuOrd que pesquisa o int pelo char na lista gerada -}
meuOrd :: Char -> [(Char,Int)] -> Int
meuOrd _ [] = error "Char nao encontrado na lista"
meuOrd x ((a,b):cs)
    | x == a = b
    | otherwise = meuOrd x cs

{-04 função que ordena uma lista de inteiros -}
ordenaLista::[Int]->[Int]
ordenaLista [] = []
ordenaLista x = ordenaListaAUX x []

ordenaListaAUX :: [Int] -> [Int] -> [Int]
ordenaListaAUX [] x = x
ordenaListaAUX (a:b) c = ordenaListaAUX b (insere a c)

insere :: Int -> [Int] -> [Int]
insere x [] = [x]
insere x (a:b)
    | x > a = a : insere x b
    | otherwise = x:(a:b)

{-05 seja o tipo [(Bool, [Int])]. 
Faça uma função que ordena [Int] quando o booleano é True. 
Também, passe o Bool para False, quando ordenar [Int]
exemplo: ordenaListaDupla [(True,[3,4,1,0,9]),(False,[]),(True,[4,3,2,1,0])]
retorna:                  [(False,[0,1,3,4,9]),(False,[]),(False,[0,1,2,3,4])]
-}

ordenaListaDupla::[(Bool, [Int])]->[(Bool, [Int])]
ordenaListaDupla [] = []
ordenaListaDupla ((b,l):xs)
    | b = (False,ordenaLista l) : ordenaListaDupla xs
    | otherwise = (False,l) : ordenaListaDupla xs
