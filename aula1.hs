type Dia = Int
type VendaR = Int

-- define o período de recursão
periodo::Int
periodo = 7

-- tabela de vendas
f :: Int -> Int
f 1 = 41
f 2 = 72
f 3 = 48
f 4 = 2
f 5 = 91
f 6 = 55
f 7 = 30
f _ = 0

{- retorna o total de vendas do período -}
totalVendas :: Int->Int
totalVendas 0 = 0
totalVendas d = f d + totalVendas (d-1)

{-encontra o dia em que mais se vendeu no período - versão 01-}
diaMaiorVenda01::Int->Int
diaMaiorVenda01 1 = 1
diaMaiorVenda01 y 
  |f y >= f (diaMaiorVenda01 (y-1)) = y
  |otherwise = diaMaiorVenda01 (y-1)

{-encontra o dia em que mais se vendeu no período - versão 02
  tem como parâmetros o período e a maior venda -}
diaMaiorVenda02::Int->Int->Int
diaMaiorVenda02 1 _ = 1
diaMaiorVenda02 y v 
  |f y == v  = y
  |otherwise = diaMaiorVenda02 (y-1) v

{- encontra o maior valor entre dois inteiros -}
maxi :: Int -> Int -> Int
maxi m n
   |m >= n = m
   |otherwise = n


{- encontra a maior venda - versão 01 
   Exercício: implemente essa função com apenas dois parâmetros e 
   fazendo uso de maxi no código interno-}
maiorVenda01:: Int->Int
maiorVenda01 0 = 0
maiorVenda01 x = maxi (maiorVenda01  (x - 1)) (f x)

maiorVenda01_Comp :: Int -> Int -> Int
maiorVenda01_Comp 0 cont = cont
maiorVenda01_Comp x cont = maiorVenda01_Comp (x-1) (cont+2)
  
  
{- encontra a maior venda - versão 02 -}
maiorVenda02::Int->Int->Int
maiorVenda02 0 y = y
maiorVenda02 x y
  |f (x-1) > y = maiorVenda02 (x-1) (f (x-1))
  |otherwise        = maiorVenda02 (x-1) y


{-Exercício - inclua em suas funções um contador para informar quantos testes
  a função executa. Isso é o mesmo que comprovar a complexidade
  Considere, neste caso, contar os testes nas funções que são chamadas internamente
-}
-- ??????????????///


