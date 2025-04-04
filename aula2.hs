import System.Win32 (xBUTTON1)
{-Assunto: Litas
  Os conceitos introdutórios sobre listas foram apresentados em sala.
  Agora, considerando os casos mais simples, com apenas listas de inteiros,
  implemente as funções abaixo, considerando os operadores ++ e :
    ++ (concatena listas)
     : (insere um elemento na lista)
-}
     
-- Feito em sala: soma elementos de uma lista de inteiros
sumList::[Int]->Int
sumList [] = 0
sumList (a:x) = a + sumList (x)

{----------- Implementar: ----------------------}

--a) localiza elemento em lista -----------------
searchList::Int->[Int]->Bool
searchList _ [] = False
searchList x (a:b)
    | x == a = True
    | otherwise = searchList x b

--b) remove todas ocorrências de um inteiro y na lista
deleteList::Int->[Int]->[Int]
deleteList _ [] = []
deleteList x (a:b)
    | x == a = deleteList x b
    | otherwise = a : deleteList x b


--informa o tamanho de uma lista ---------------
lenghtList::[Int]->Int
lenghtList [] = 0
lenghtList (a:b) = 1 + lenghtList b 

--contar ocorrência de um elemento em uma lista --
contList::Int->[Int]->Int
contList _ [] = 0
contList x (a:b)
    | a == x = 1 + contList x b
    | otherwise = contList x b


--inverte elementos da lista
reverseList:: [Int]->[Int]
reverseList [] = []
reverseList (a:b) = reverseList b ++ [a] 

-------------------------------------------------------------
{- Exercícios
     Implementar as funções: 
       myHead que recebe uma lista x e retorna a cabeça de x
       myTail que recebe uma lista x e retorna a lista x sem a cabeça
       myLast que recebe uma lista x e retorna o último elemento de x
       myInit que recebe uma lista x e retorna a lista x sem o último elemento
-}

--retorna cabeça da lista
myHead :: [Int]->Int
myHead (a:b) = a

--retorna a cauda da lista
myTail :: [Int]->[Int] 
myTail (a:b) = b 

--retorna ultimo elemento da lista
myLast :: [Int]->Int
myLast [x] = x
myLast (a:b) = myLast b

--Retorna sem ultimo elemento
myInit :: [Int]->[Int]
myInit [] = []
myInit [x] = []
myInit (a:b) = a : myInit b