---    exemplo.hs
---
idade :: Int  -- Um valor inteiro constante
idade = 17

maiorDeIdade :: Bool   -- Usa a definição de idade
maiorDeIdade = (idade>=18) 

quadrado :: Int -> Int  -- função que eleva num ao quadrado
quadrado x = x * x       

mini :: Int -> Int -> Int  -- função que mostra
mini a b                   -- o menor entre
    | a <= b  = a          -- dois valores
    | otherwise = b