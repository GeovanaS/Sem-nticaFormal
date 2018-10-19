-- Exercícios sobre Tipos Algébricos Recursivos

data Temperatura = Frio | Calor
  deriving(Eq,Show)

data Estacao = Verao | Outono | Inverno | Primavera
  deriving(Eq,Show)  

tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo Inverno = Frio
tempo_ = Frio

data Forma = Circulo Float |Retangulo Float Float
   deriving(Eq,Show)

redondo :: Forma -> Bool
redondo (Circulo x) = True
redondo (Retangulo x y) = False

area :: Forma -> Float
area (Circulo r) = pi * r *r
area (Retangulo b a) = b * a

-- uma arvore binaria de inteiros
data Arvore = Folha Int | Nodo Int Arvore Arvore
  deriving(Eq,Show)
-- Uma arvore binaria polimorfica:
-- data ArvoreP a = Folha | Nodo a (Arvore a) (Arvore a)

-- exemplo
--minhaArvore :: Arvore
--minhaArvore = Nodo 10 (Nodo 14 (Nodo 1 Folha Folha) Folha) Folha

arv1 :: Arvore
arv1 = Nodo 2 (Nodo 3 (Folha 4) (Folha 2)) (Folha 7)

somaArvore :: Arvore -> Int
somaArvore (Folha n) = 0
somaArvore (Nodo n a1 a2) = n + somaArvore a1 + somaArvore a2

-- 1. Defina uma função que multiplique por 2 os inteiros em uma árvore
multArvore :: Arvore -> Int
multArvore (Folha n) = n * 2
multArvore (Nodo n a1 a2) = (n * 2) + multArvore a1  + multArvore a2

-- 2. Defina uma função que conte quantos elementos existem em uma árvore
contArv :: Arvore -> Int
contArv (Folha n) = 1
contArv (Nodo n a1 a2) = 1 + contArv a1 + contArv a2

-- 3. Defina uma função que ache o maior elemento de uma árvore
maiorArv :: Arvore -> Int
maiorArv (Folha n) = n
maiorArv (Nodo n a1 a2) 
  | n > maiorArv a1 && n > maiorArv a2 = n 
  | maiorArv a1 > maiorArv a2 = maiorArv a1
  | otherwise = maiorArv a2

-- 4. Defina a função que diz se um inteiro ocorre dentro de uma árvore
ocorreArv :: Arvore -> Int -> Bool
ocorreArv (Folha num) n = num == n 
ocorreArv (Nodo num a1 a2) n = (n == num) || (ocorreArv a1 n)  || (ocorreArv a2 n)

-- 5. Defina uma função que diz quantas vezes um inteiro ocorre dentro de uma árvore
vezesOcorrencia :: Int -> Arvore -> Int
vezesOcorrencia n (Folha num) = if(n==num) then 1 else 0
vezesOcorrencia n (Nodo num a1 a2)
  | (n == num) = 1 + (vezesOcorrencia n a1) + (vezesOcorrencia n a2)
  | otherwise = (vezesOcorrencia n a1) + (vezesOcorrencia n a2) 


-- 6. Uma árvore refletida é uma árvore com seus ramos esquerdos e direitos trocados. Defina uma função refleteArvore
refleteArvore :: Arvore -> Arvore
refleteArvore (Folha n) = (Folha n)
refleteArvore (Nodo n a1 a2) = Nodo n (refleteArvore a2) (refleteArvore a1)

-- 7. Defina uma função que calcule a altura de uma árvore
altArv :: Arvore -> Int
altArv (Folha n) = 0
altArv (Nodo n a1 a2) = 1 + max(altArv a1) (altArv a2)

-- 8. Defina uma função que transfore uma árvore em uma lista
listaArv :: Arvore -> [Int]
listaArv (Folha n) = [n]
listaArv (Nodo n a1 a2) = [n] ++ (listaArv a1) ++ (listaArv a2)

-- 9.  Defina a função mapTree que aplica uma função a todos os inteiros de todos os nós de uma árvore.
mapTree :: (Int -> Int) -> Arvore -> Arvore
mapTree n (Folha num) = (Folha num)
mapTree n (Nodo num a1 a2) = Nodo (n num) (mapTree n a1) (mapTree n a2)