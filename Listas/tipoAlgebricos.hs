data Temperatura = Frio | Calor
  deriving(Eq,Show)

data Estacao =  Verao | Outono | Inverno | Primavera
  deriving(Eq,Show)

tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo _ = Frio

data Forma = Circulo Float |Retangulo Float Float
  deriving(Eq,Show)

redondo :: Forma -> Bool
redondo (Circulo x) = True
redondo (Retangulo x y) = False

area :: Forma -> Float
area (Circulo r) = pi * r *r
area (Retangulo b a) = b * a

-- Uma arvore binaria de inteiros:
data Arvore = Folha Int | Nodo Int Arvore Arvore
  deriving(Eq,Show)

arv1 :: Arvore
arv1 = Nodo 2 (Nodo 3 (Folha 4) (Folha 2)) (Folha 7)

somaArvore :: Arvore -> Int
somaArvore (Folha n) = 0
somaArvore (Nodo n a1 a2) = n + somaArvore a1 + 

somaArvore a2
