--
-- Lista 1 de exercícios de Haskell 
--

-- Exercício 1: Escreva a função osQuatroSaoIguais que possui tipo
-- Int -> Int -> Int -> Int -> Bool
-- que retorna True se seus quatro argumentos são iguais
osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais x y z w = (x == y) && (y == z) && (z == w)

-- Exercício 2: Defina a função quantosSaoIguais :: Int -> Int -> Int -> Int que
-- conta quantos argumentos iguais a função recebeu
quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais x y z 
   | (x == y) && (y == z) = 3
   | (x /= y) && (y /= z) = 0
   | otherwise = 2

-- Exercício 3: Defina a função todosDiferentes :: Int -> Int -> Int -> Bool
-- que retorna True se todos os seus argumentos são diferentes. Obs: m /= n retorna True se m e n são diferentes
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes x y z
 | (x /= y) && (y /= z) && (x /= z) = True
 | otherwise = False

-- Exercício 4:  Defina um conjunto de testes para a função todosDiferentes
-- todosDiferentes 0 1 2
-- todosDiferentes 2 1 2
-- todosDiferentes 2 2 2
-- todosDiferentes 1 2 2
-- todosDiferentes 2 2 1


-- Exercício 5: O que está errado com a seguinte definição de todosDiferentes:
-- todosDiferentes n m p = ( ( n/=m ) && ( m/=p ) )
-- O conjunto de testes que você definiu na questão anterior funciona com
-- esta definição?
-- Não funcionaria, pois não testa se n é diferente de p.

-- Exercício 6: Defina a função 
-- todosIguais :: Int -> Int -> Int -> Bool
todosIguais :: Int -> Int -> Int -> Bool
todosIguais x y z
 | (x == y) && (y == z) = True
 | otherwise = False

-- Exercício 7: Escreva uma definição de quantosSaoIguais que use a função todosDiferentes
-- e a função todosIguais
quantosSaoIguais2 :: Int -> Int -> Int -> Int
quantosSaoIguais2 x y z
 | todosDiferentes x y z = 0
 | todosIguais x y z = 3
 | otherwise = 2

-- Exercício 8: Defina a função elevadoDois :: Int -> Int que recebe um argumento n
-- e devolve como resposta n^2
elevadoDois :: Int -> Int
elevadoDois n = n ^ 2

-- Exercício 9: Defina a função elevadoQuatro :: Int -> Int que recebe um argumento n
-- e devolve como resposta n^4. Use elevadoDois para definir elevadoQuatro
elevadoQuatro :: Int -> Int
elevadoQuatro n = elevadoDois(elevadoDois n)

-- Exercício 10: Supondo que exista uma função vendas: 
-- vendas :: Int -> Int 
-- que devolve a venda semanal de uma loja (ex: vendas 0 devolve as vendas na semana 0, 
-- vendas 1 devolve as vendas na semana 1, etc. Implemente uma função chamada vendaTotal, 
-- que recebe um argumento n e calcula todas as vendas da semana 0 até a semana n.
-- Observe que essa função deve ser recursiva. 
-- Exemplo de calculo: As vendas da semana 0 até a semana 2, podem ser calculados usando a seguinte formula:
-- vendas 0 + vendas 1 + vendas 2
vendas :: Int -> Int
vendas 0 = 33
vendas 1 = 22
vendas 2 = 11
vendas_ = 25

vendaTotal:: Int -> Int
vendaTotal 0 = vendas 0
vendaTotal n = vendas n + vendaTotal(n-1)



