--
-- Lista 2 de Exercícios de Haskell
--

-- Exercício 1: Defina uma função max :: Int -> Int -> Int
-- que retorna o maior entre dois números
maior :: Int -> Int -> Int
maior x y 
 | (x > y) = x
 | otherwise = y

-- Exercício 2: Usando a função max, defina uma função maiorVenda 
-- que recebe um argumento numérico n, e calcule a maior venda em
-- uma semana entre 0 e n
vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 11
vendas 2 = 11
vendas 3 = 22
vendas 4 = 55
vendas _ = 25

maiorVenda :: Int -> Int 
maiorVenda n 
 | (n == 0) = vendas 0
 | otherwise = maior(vendas n) (maiorVenda(n-1))


-- Exercício 3: Defina uma função maxVenda que recebe um argumento numérico n,
-- e calcule a semana, entre 0 e n, que teve o maior número de vendas. 
-- Essa função deve usar maiorVenda em sua definição
maxVenda :: Int -> Int 
maxVenda n 
 | (n == 0) = 0
 | (maiorVenda n == vendas n) = n
 | otherwise = maxVenda(n-1)

-- Exercício 4: Defina uma função zeroVendas que recebe um argumento numérico n,
-- e que calcula qual das semanas entre 0 e n teve vendas igual a 0. Se nenhuma
-- semana teve vendas igual a 0 a função retorna -1
zeroVendas :: Int -> Int
zeroVendas n
 | (vendas n == 0) = n
 | otherwise = -1

-- Exercício 5: Usando a definição anterior como guia, defina uma função que receba
-- um valor s e uma semana n, e devolva qual das semanas entre 0 e n teve vendas
-- iguais a s
vendasIguais :: Int -> Int -> Int
vendasIguais s n
 | (vendas n == s) = n
 | otherwise = vendasIguais s(n-1)


-- Exercício 6: Como você usaria a função anterior para definir a função zeroVendas
zeroVendas1 :: Int -> Int -> Int
zeroVendas1 n m
  | (vendas n == 0) && (vendas m == 0) = 0
  | (vendasIguais n m == vendas n)  = -1
  | otherwise = -1

-- Exercício 7: As funções definidas até agora operam em um periodo entre 0 e n. 
-- Defina versões alternativas dessas funções que trabalhem em um periodo entre m e n,
-- assumindo que n sempre é maior que m.
maiorVenda2 :: Int -> Int -> Int
maiorVenda2 m n
 | (m == n) = vendas n
 | otherwise = maior(vendas n) (maiorVenda2 m (n-1))

maxVenda2 :: Int -> Int -> Int
maxVenda2 n m
 | (n == m) = n
 | (maiorVenda2 n m == vendas n) = n
 | otherwise = maxVenda2 m(n-1)

zeroVendas2 :: Int -> Int -> Int
zeroVendas2 n m
 | (vendas n == 0) = n
 | (m == n) = -1
 | otherwise = zeroVendas2 m (n-1)

vendasIguais2 :: Int -> Int -> Int -> Int
vendasIguais2 s n m
 | (vendas n == s) = n
 | (m == n) = -1
 | otherwise = vendasIguais2 s m(n-1)


-- Exercício 8: O fatorial de um número positivo n é 
-- 1 * 2 * ... * (n-1) * n
-- Defina uma função fatorial em Haskel
fatorial :: Int -> Int
fatorial n
  | (n == 0) = 1
  | (n == 1) = 1
  | otherwise = n * fatorial(n - 1)

-- Exercício 9: Defina uma função que receba dois argumentos m e n e retorne o produto
-- m * (m+1) * ... * (n-1) * n
produto :: Int -> Int -> Int
produto m n
 | (n <= 0)  = 1
 | (m == n) = n
 | otherwise =  n * produto m (n-1) 


-- Exercício 10: Considere a sequência fibonacci de nÚmeros: 0, 1, 1, 2, 3, 5, ... cujos
-- dois primeiros valores são 0 e 1, e os valores seguintes são sempre a soma dos dois valores anteriores 
-- Escreva em Haskell a função fib sendo que fib n devolve o número que esta na posição n da sequência fibonacci
fib :: Int -> Int
fib n 
 | (n == 0) = n
 | (n == 1) = n
 | otherwise = fib(n-1)+fib(n-2)
