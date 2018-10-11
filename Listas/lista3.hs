somaLista :: [Int] -> Int
somaLista [] = 0
somaLista(a:x) = a + somaLista x

dobraLista :: [Int] -> [Int]
dobraLista [] = []
dobraLista(x:xS) = x * x : dobraLista xS

tamanho :: [Int] -> Int
tamanho [] = 0
tamanho(x:xC) = 1 + tamanho xC

produtoLista :: [Int] -> Int
produtoLista [] = error "lista vazia"
produtoLista[x] = x
produtoLista(x:xS) = x * produtoLista xS

andLista :: [Bool] -> Bool
andLista[] = True
andLista(x:xS) = x && andLista xS
