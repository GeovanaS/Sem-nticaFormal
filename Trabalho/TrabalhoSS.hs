import Estado

-- Expressões Aritméticas
data AExp = Num Int
     |Var String
     |Som AExp AExp
     |Sub AExp AExp
     |Mul AExp AExp
  deriving(Show)

-- Expressões Booleanas
data BExp = TRUE
     | FALSE
     | Not BExp
     | And BExp BExp
     | Or  BExp BExp
     | Ig  AExp AExp
   deriving(Show)

-- Comandos
data CExp = While BExp CExp
     | If BExp CExp CExp
     | Seq CExp CExp
     | Atrib AExp AExp
     | Skip
   deriving(Show)                


 -- Semântica de Expressões Aritmeticas    
aSmallStep :: (AExp,Estado) -> (AExp,Estado)
 -- Var
aSmallStep (Var x,s) = (Num (procuraVar s x),s)

-- Soma3 
aSmallStep (Som (Num x) (Num y), s) = (Num (x+y),s)
-- Soma2
aSmallStep (Som (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
                                 in (Som (Num x) ef,s)
-- Soma1                                 
aSmallStep (Som e1 e2,s)  = let (ef,_) = aSmallStep (e1, s)
                            in (Som ef e2,s)

-- Sub3
aSmallStep (Sub (Num x) (Num y), s) = (Num (x-y),s)

-- Sub2
aSmallStep (Sub (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
                                 in(Sub(Num x) ef,s)

-- Sub1                            
aSmallStep (Sub e1 e2,s) =  let (ef,_) = aSmallStep (e1, s)
                            in (Sub ef e2,s)
                        
-- Mul3
aSmallStep (Mul (Num x) (Num y), s) = (Num (x*y),s)

-- Mul2                            
aSmallStep (Mul (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
                                 in (Mul (Num x) ef,s)   

-- Mul1
aSmallStep (Mul e1 e2,s) = let (ef,_) = aSmallStep (e1, s)
                           in (Mul ef e2,s)

interpretA :: (AExp,Estado) -> (AExp,Estado)
interpretA (a,s) = if isFinalA a then (a,s) else interpretA (aSmallStep (a,s))

isFinalA :: AExp -> Bool
isFinalA (Num a) = True
isFinalA x = False


-- Semântica de Expressões Booleanas

bSmallStep :: (BExp,Estado) -> (BExp,Estado)
-- Not3
bSmallStep (Not FALSE,s)      = (TRUE,s)
-- Not2
bSmallStep (Not TRUE,s)       = (FALSE, s)
-- Not1
bSmallStep (Not b, s) = let (bn,sn) = bSmallStep (b,s)
                        in (Not bn ,sn)

-- And3
bSmallStep (And FALSE b2,s) = (FALSE,s)                        
-- And2                   
bSmallStep (And TRUE b2,s)  = (b2,s)
-- And1
bSmallStep (And b1 b2,s)    = let (bn,sn) = bSmallStep (b1,s)
                              in (And bn b2,sn)

-- Ig3
bSmallStep (Ig (Num x) (Num y),s) = if(x==y) then(TRUE,s) else (FALSE,s)
-- Ig2
bSmallStep (Ig (Num x) e2,s) = let(ef,s1) = aSmallStep(e2,s)
                               in(Ig (Num x) ef,s1)
-- Ig1
bSmallStep (Ig e1 e2,s)  = let(en,sn) = aSmallStep(e1,s)
                           in(Ig en e2,sn)

-- Exemplo de entrada
-- *Main> bSmallStep(Ig (Num 6) (Som (Var "x") (Num 5)),[("x",6),("y",0),("z",0)])
-- *Main> bSmallStep(Ig (Num 8) (Num 6),[("x",0),("y",0)])                          


-- Or3
bSmallStep (Or FALSE b2,s )  = (b2,s)
-- Or2
bSmallStep (Or TRUE b2,s) = (TRUE,s)
-- Or1
bSmallStep (Or b1 b2,s)    = let (bn,sn) = bSmallStep (b1,s)
                              in (Or bn b2,sn)


interpretB :: (BExp,Estado) -> (BExp,Estado)
interpretB (b,s) = if isFinalB b then (b,s) else interpretB (bSmallStep (b,s))

isFinalB :: BExp -> Bool
isFinalB TRUE = True
isFinalB FALSE = True
isFinalB x = False


-- Semântica de Comandos

cSmallStep :: (CExp,Estado) -> (CExp,Estado)

-- If3
cSmallStep (If FALSE c1 c2,s) = (c2,s)

-- If2    
cSmallStep (If TRUE c1 c2,s) = (c1,s)

-- If2
cSmallStep (If b c1 c2,s) = let(bn,sn) = bSmallStep(b,s)
                            in(If bn c1 c2,sn)

-- Seq2
cSmallStep (Seq Skip c,s) = (c,s)
-- Seq1
cSmallStep (Seq c1 c2,s)  = let(cn,sn) = cSmallStep(c1,s)
                            in(Seq cn c2,sn)


-- Atrib2
cSmallStep (Atrib (Var x) (Num n),s) = (Skip, mudaVar s x n)

-- Atrib1 
cSmallStep (Atrib (Var x) e,s) = let(en,sn) = aSmallStep(e,s)
                                 in(Atrib (Var x) en,sn)



interpretC :: (CExp,Estado) -> (CExp,Estado)
interpretC (c,s) = if isFinalC c then (c,s) else interpretC (cSmallStep (c,s))

isFinalC :: CExp -> Bool
isFinalC Skip = True
isFinalC x = False 



meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]


exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))

-- RODANDO O EXEMPLO:
-- Hugs> interpretA (exemplo, meuEstado)

exemplo2 :: BExp
exemplo2 = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE)

-- *Main> interpretB (exemplo2,meuEstado)
-- (TRUE,[("x",3),("y",0),("z",0)])


-- Dupla Atribuição



-- Repeat Until



-- For
