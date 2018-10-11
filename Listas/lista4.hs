-- Lista 4 de exercícios de Haskell


-- Exercicio 1: defina uma função que retorna um booleando que diz se o inteiro esta na lista
membro :: [Int] -> Int -> Bool
membro [] n = False
membro(x:xS) n
    | n == x = True
    | otherwise = membro xS n

-- Exercício 2: Implemente a função que conta o número de vezes que o inteiro aparece na lista
membroNum :: [Int] -> Int -> Int
membroNum [] n = 0
membroNum (x:xS) n
         |n == x = 1 + membroNum xS n
         |otherwise = membroNum xS n

-- Exercício 3:  Defina a função membro usando a função membroNum
membro2 :: [Int] -> Int -> Bool
membro2 (x:xS) n
        | membroNum (x:xS) n==0 = False
        | otherwise = True
 
-- Exercicio 4: Implemente uma função que retorna uma lista com os números que aparecem apenas uma vez na lista argumento
unico :: [Int] -> [Int]
unico [] = []
unico (x:xS)
      | (membroNum (x:xS) x == 1) = x : unico xS
      | (membroNum (x:xS) x > 1) = unico (removeLista(x:xS) x)

removeLista :: [Int] -> Int -> [Int]
removeLista [] n = []
removeLista (x:xS) n 
      | n == x = removeLista xS n
      | otherwise = x:removeLista xS n

-- Exercicio 5: Se a lista argumento para membro está ordenada, não é necessário percorrer toda a lista para saber se o elemento está presente na lista. 
-- Implemente uma nova definição de membro, que use iSort.
membro3 :: [Int] -> Int -> Bool
membro3 x n = membro (iSort x) n

insertionS :: Int -> [Int] -> [Int]
insertionS n [] = [n];
insertionS n (x:xS)
	| n >= x = x : insertionS n xS
	| otherwise = [n,x] ++ xS

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xS) = insertionS x (iSort xS)