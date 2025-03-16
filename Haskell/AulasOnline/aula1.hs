-- Contar degrais
contarEscada 0 = 0
contarEscada n = 1 + contarEscada(n - 1)

-- Fatorial
fatorial :: Int -> Int
fatorial 0 = 1  -- Caso base
fatorial n = n * fatorial (n - 1)   --Recursividade

-- Soma de 1 ate N 
somaAteN :: Int -> Int
somaAteN 0 = 0
somaAteN n = 1 + somaAteN(n - 1)

-- Potencia (base^expoente)
potencia :: Int -> Int -> Int
--potencia n 0 = 1
potencia n 1 = n
potencia n m = n * potencia n (m - 1)

-- Produto de dois numeros sem usar * 
multiplica :: Int -> Int -> Int
multiplica n 1 = n 
multiplica n 0 = 0
multiplica n m = n + multiplica n (m - 1)

-- Verificar se é par (sem usar mod)
ehPar :: Int -> Bool
ehPar 0 = True
ehPar 1 = False
ehPar n = ehPar (n - 2) 
    
-- Contagem regressiva
contagemRegressiva :: Int -> IO ()
contagemRegressiva 0 = print 0
contagemRegressiva n = do 
    print n
    contagemRegressiva (n - 1)
    
