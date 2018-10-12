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
data Arvore = Folha | Nodo Int Arvore Arvore
-- Uma arvore binaria polimorfica:
-- data ArvoreP a = Folha | Nodo a (Arvore a) (Arvore a)

-- exemplo
minhaArvore :: Arvore
minhaArvore = Nodo 10 (Nodo 14 (Nodo 1 Folha Folha) Folha) Folha

somaArvore :: Arvore -> Int 
somaArvore Folha = 0
somaArvore (Nodo n a1 a2) = n + somaArvore a1 + somaArvore a2

-- 1. Defina uma função que multiplique por 2 os inteiros em uma árvore
multArvore :: Arvore -> Arvore
multArvore Folha = Folha
multArvore (Nodo n a1 a2) = Nodo (2*n) (multArvore a1) (multArvore a2)


