import Data.Char

{-01 função que retorna lista de duplas com char e posição na ASCII -}
listaDuplaCharInt:: Int-> [(Char,Int)]

{-02 função meuChar que pesquisa um char pelo int na lista gerada -}

{-03 função meuOrd que pesquisa o int pelo char na lista gerada -}

{-04 função que ordena uma lista de inteiros -}
ordenaLista::[Int]->[Int]
ordenaLista [] = []

{-05 seja o tipo [(Bool, [Int])]. 
Faça uma função que ordena [Int] quando o booleano é True. 
Também, passe o Bool para False, quando ordenar [Int]
exemplo: ordenaListaDupla [(True,[3,4,1,0,9]),(False,[]),(True,[4,3,2,1,0])]
retorna:                  [(False,[0,1,3,4,9]),(False,[]),(False,[0,1,2,3,4])]
-}

ordenaListaDupla::[(Bool, [Int])]->[(Bool, [Int])]
ordenaListaDupla [] = []
