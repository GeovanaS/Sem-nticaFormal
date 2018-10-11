somaLista :: [Int] -> Int
somaLista [] = 0
somaLista(a:x) = a + somaLista x

-- Exercicio 1: Implemente a função que dobra o valor de todos os elementos de uma lista
dobraLista :: [Int] -> [Int]
dobraLista [] = []
dobraLista(x:xS) = x * x : dobraLista xS

-- Exercicio 2: Implemente a função que conta o número de elementos de uma lista
tamanho :: [Int] -> Int
tamanho [] = 0
tamanho(x:xC) = 1 + tamanho xC

-- Exercicio 3: Implemente a função que implementa o produto de uma lista de inteiros.
produtoLista :: [Int] -> Int
produtoLista [] = error "lista vazia"
produtoLista[x] = x
produtoLista(x:xS) = x * produtoLista xS

-- Exercicio 4: Implemente a função que faz um and(&&) entre todos os elementos de uma lista 
andLista :: [Bool] -> Bool
andLista[] = True
andLista(x:xS) = x && andLista xS

-- Exercicio 5: Implemente a função que transforma uma lista de lista de inteiros em uma lista única de inteiros.
concatLista :: [[Int]] -> [Int]
concatLista[] = []
concatLista(x:xS) = x ++ concatLista xS

-- Exercicio 6: Implemente a função de inverte a Lista
inverteLista :: [Int] -> [Int]
inverteLista [] = []
inverteLista(x:xS) = inverteLista xS ++ [x]