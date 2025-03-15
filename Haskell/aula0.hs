--Nomes de constantes
type Vendas = Int
type Dia = Int

--Elevar um número ao quadrado
quadrado :: Int -> Int
quadrado x = x * x

--Lista de valores de vendas
f :: Dia -> Vendas
f 1 = 2
f 2 = 15
f 3 = 7
f 4 = 11
f 5 = 3
f 6 = 4
f 7 = 1
f x = 0

--Encontrar dia de mais vendas
maiorV :: Int -> Int -> Int
maiorV 0 v = v
maiorV d v
    |f d > v   = maiorV (d-1) (f d)
    |otherwise = maiorV (d-1) v

--Simplificar a chamada de maiorVenda
maiorVenda :: Int 
maiorVenda = maiorV 7 0
