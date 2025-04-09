{- Assunto: listas e tuplas -}
periodo::Int
periodo = 7

maxi :: Int -> Int -> Int
maxi m n
   |m >= n = m
   |otherwise  = n


-- tabela de vendas
vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 41
vendas 2 = 72
vendas 3 = 48
vendas 4 = 0
vendas 5 = 91
vendas 6 = 55
vendas 7 = 30

{- 01 função que retorna uma lista de vendas -}
listaVendas :: Int -> [Int]
listaVendas 0 = []
listaVendas x = listaVendas (x-1) ++ [vendas x]

{- 02 função que retorna [[Int]] com listas de dia e venda -}
listaDiaVendas :: Int-> [[Int]]
listaDiaVendas 0 = []
listaDiaVendas x = listaDiaVendas (x-1) ++ [[x, vendas x]]

----------------------------------------------------------
{- 03 função que ordena uma lista de inteiros -}
ordenaLista::[Int]->[Int]
ordenaLista [] = []
ordenaLista (x:xs) = insere x (ordenaLista xs)


insere :: Int->[Int]->[Int]
insere x [] = [x]
insere x (y:ys) 
   | x > y = y:insere x ys 
   | otherwise = x:(y:ys)


-------------------------------------------------------------------------
{- 04 função que ordena [[Int]] pelo primeiro Int de cada lista  -}
ordenaListaLista :: [[Int]]->[[Int]]
ordenaListaLista [] = [] 
ordenaListaLista (x:xs) = insere2 x (ordenaListaLista xs)

insere2 :: [Int]->[[Int]]->[[Int]]
insere2 x [] = [x]
insere2 (x:xs) ((z:zs):ys) 
   | x > z = (z:zs):insere2 (x:xs) ys 
   | otherwise = (x:xs):((z:zs):ys)

---------------------------------------------------------------------------
{- 05 função que ordena as listas internas de [[Int]] e, em seguida, ordena a [[Int]] -}
ordenaLILE ::[[Int]] ->[[Int]]
ordenaLILE x = ordenaListaLista (auxLILE x)


auxLILE :: [[Int]]->[[Int]]
auxLILE [] = []
auxLILE (y:ys) = (ordenaLista y):(auxLILE ys)


-----------  tuplas --------------------------------------------------------
{- 06 função que gera uma lista de tuplas com dia e venda -}
--listaTuplaDiaVenda :: Int-> [(Int, Int)]

{- 07 função que gera o total de vendas-}
--totalVendasT::[(Int, Int)] -> Int

{- 08 função que retorna a maior venda -}
--maiorVendaT::[(Int, Int)] -> Int

{- 09 função que retorna os dias das maiores vendas -}
