import Data.Char

-- 1) Soma impares
somaImpares :: [Int] -> Int
somaImpares [] = 0
somaImpares (x:xs)
    | x `mod` 2 /= 0 = x + somaImpares xs
    | otherwise = somaImpares xs

-- 2) Conta ocorrencia em lista
contaOcorrencias :: [Int] -> Int -> Int
contaOcorrencias [] _ = 0
contaOcorrencias (x:xs) y
    | x == y = 1 + contaOcorrencias xs y
    | otherwise = contaOcorrencias xs y

-- 3) Remover valores duplicados
removerDuplicatas :: [Int] -> [Int]
removerDuplicatas [] = []
removerDuplicatas (x:xs)
    | maisDeUm xs x = x : removerDuplicatas(removeRepetidos x xs)
    | otherwise = x : removerDuplicatas xs

maisDeUm :: [Int] -> Int -> Bool
maisDeUm [] _ = False
maisDeUm (x:xs) y 
    | x == y = True
    | otherwise = maisDeUm xs y

removeRepetidos :: Int -> [Int] -> [Int]
removeRepetidos _ [] = []
removeRepetidos x (y:ys)
    | x == y = removeRepetidos x ys
    | otherwise = y : removeRepetidos x ys

-- 4) Funcao que gera numeros primos ate n
primosAteN :: Int -> [Int]
primosAteN 0 = []
primosAteN x = naoDivisores x 2 

naoDivisores :: Int -> Int -> [Int]
naoDivisores x y
    | y == x = []
    | temDivisor y (y-1)  = naoDivisores x (y+1)
    | otherwise = y : naoDivisores x (y+1)

temDivisor :: Int -> Int -> Bool
temDivisor x y
    | y == 1 = False
    | x `mod` y == 0 = True
    | otherwise = temDivisor x (y-1)


-- Exercicio similar ao 37)
type RA = Int
type Nome = String
type Nota = Float
type Faltas = Int
type Avaliacao = (RA, Nota)
type Frequencia = (RA, Faltas)
type Aluno = (RA, Nome)

avaliacoes :: [Avaliacao]
avaliacoes = [(1, 7.5), (2, 4.0), (3, 9.0), (4, 6.5)]

frequencias :: [Frequencia]
frequencias = [(1, 2), (2, 10), (3, 0), (4, 5)]

alunos :: [Aluno]
alunos = [(1, "Joana"), (2, "Carlos"), (3, "Marina"), (4, "Paulo")]

-- a) Recebe o numero do aluno, e ve se sua nota eh maior ou igual a 6
aprovadoNota :: Int -> [Avaliacao] -> Bool
aprovadoNota _ [] = error "Aluno nao encontrado"
aprovadoNota x (y:ys)
    | x == (fst y) && snd(y) >= 6 = True
    | x == (fst y) && snd(y) < 6 = False
    | otherwise = aprovadoNota x ys

-- b) Se o alunoo foi aprovado por frequencia
aprovadoFrequencia :: Int -> [Frequencia] -> Bool
aprovadoFrequencia _ [] = error "Aluno nao encontrado"
aprovadoFrequencia x (y:ys)
    | x == (fst y) && snd y <= 5 = True
    | x == (fst y) && snd y > 5 = False
    | otherwise = aprovadoFrequencia x ys

-- c) 
situacaoAluno :: Int -> [Avaliacao] -> [Frequencia] -> String
situacaoAluno x av fq
    | aprovadoNota x av && aprovadoFrequencia x fq = "Aprovado"
    | not (aprovadoNota x av) && aprovadoFrequencia x fq = "Reprovado por nota" 
    | aprovadoNota x av && not(aprovadoFrequencia x fq) = "Reprovado por frequencia"
    | otherwise = "Reprovado por nota e por frequencia"

-- d) Resumo de todos os alunos
resumoTurma :: [Aluno] -> [Avaliacao] -> [Frequencia] -> [(Nome, String)]
resumoTurma [] _ _  = []
resumoTurma (x:xs) av fq = (snd x, situacaoAluno (fst x) av fq) : resumoTurma xs av fq

--------------------------------------------------------------------------------------

type ClienteID = Int
type ProdutoID = Int
type Quantidade = Int
type Estoque = [(ProdutoID, Nome, Quantidade)]
type Carrinho = [(ClienteID, ProdutoID, Quantidade)]

estoque :: Estoque
estoque = [(101, "Mouse", 10), (102, "Teclado", 5), (103, "Monitor", 2)]

carrinho :: Carrinho
carrinho = [(1, 101, 2), (2, 103, 1), (1, 102, 1), (2,101,15)]

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

trd3 :: (a,b,c) -> c
trd3 (_,_,x) = x

-- a) Verifica se um produto tem em quantidade suficiente no estoque
disponivel :: ProdutoID -> Quantidade -> Estoque -> Bool
disponivel _ _ [] = error "Produto nao encontrado"
disponivel id qtd (x:xs)
    | (id == fst3 x) && (trd3 x >= qtd) = True
    | (id == fst3 x) && (trd3 x < qtd) = False
    | otherwise = disponivel id qtd xs

-- b) Retorna a quantidade atual de um produto no estoque.
quantidadeProduto :: ProdutoID -> Estoque -> Quantidade
quantidadeProduto _ [] = error "Produto nao encontrado"
quantidadeProduto id (x:xs)
    | fst3 x == id = trd3 x
    | otherwise = quantidadeProduto id xs

{- c) Verifica se um item no carrinho de um cliente é válido 
(isto é, pode ser atendido com o estoque atual). -}
pedidoValido :: ClienteID -> ProdutoID -> Carrinho -> Estoque -> Bool
pedidoValido _ _ [] _ = error "Cliente nao encontrado"
pedidoValido _ _ _ [] = error "Produto nao encontrado"
pedidoValido idC idP (x:xs) y 
    | (idC == fst3 x) && (idP == snd3 x) && (trd3 x <= (quantidadeProduto idP y)) = True
    | (idC == fst3 x) && (idP == snd3 x) && (trd3 x > (quantidadeProduto idP y)) = False
    | otherwise = pedidoValido idC idP xs y

-- d) Lista de pedidos invalidos no carrinho
pedidosInvalidos :: Carrinho -> Estoque -> [(ClienteID, ProdutoID)]
pedidosInvalidos [] _ = []
pedidosInvalidos (x:xs) y
    | pedidoValido (fst3 x) (snd3 x) (x:xs) y = pedidosInvalidos xs y
    | otherwise = (fst3 x, snd3 x) : pedidosInvalidos xs y


