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

