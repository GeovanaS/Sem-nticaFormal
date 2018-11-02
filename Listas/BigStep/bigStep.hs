import Estado

-- Expressões Aritméticas
data E = Num Int | Soma E E | Sub E E | Mult E E | Var String
     deriving(Eq,Show)

-- Expressões Booleanas
data B = TRUE | FALSE | Eq B B | Leq B B | Not B | And B B | Or B B
     deriving(Eq,Show)

-- Comandos
data C = SKIP | Atrib E E | Seq C C | If B C C | While B C
     deriving(Show)  

-- Semântica de Expressões Aritmeticas

bigStepE :: (E,Estado) -> (Int,Estado)
-- Num
bigStepE (Num n,s) = (n,s)

-- Var
bigStepE (Var x,s) = (procuraVar s x,s)
--bigStepE (Var "x",[("x",0)])
-- Soma
bigStepE (Soma e1 e2,s) = let (n1,s1) = bigStepE (e1,s); 
                                                 (n2,s2) = bigStepE(e2,s)
                                                 in(n1+n2,s)
-- bigStepE (Soma (Num 5) (Num 2),[("x",0)])

-- Sub
bigStepE (Sub e1 e2,s) = let (n1,s1) = bigStepE (e1,s); 
                                                 (n2,s2) = bigStepE(e2,s)
                                                 in(n1-n2,s)

-- Mult
bigStepE (Mult e1 e2,s) = let (n1,s1) = bigStepE (e1,s); 
                                                 (n2,s2) = bigStepE(e2,s)
                                                 in(n1*n2,s)

-- Semântica de Expressões Booleanas
bigStepB :: (B,Estado) -> (Bool,Estado)
-- TRUE
bigStepB (TRUE,s) = (True,s)
-- FALSE
bigStepB (FALSE,s) = (False,s)
-- NOT
bigStepB (Not b,s) = let (b1,s1) = bigStepB(b,s)
                     in (not b1,s1)  
-- AND
bigStepB (And b1 b2,s) = let(e1,s1) = bigStepB(b1,s);
                                     (e2,s2) = bigStepB(b2,s)
                                     in(e1 && e2, s)
-- OR 
bigStepB (Or b1 b2,s) = let(e1,s1) = bigStepB(b1,s);
                                     (e2,s2) = bigStepB(b2,s)
                                     in(e1 || e2, s)
-- EQ
bigStepB (Eq e1 e2,s) = let(n1,s1) = bigStepB(e1,s);
                                     (n2,s2) = bigStepB(e2,s)
                                     in(e1 == e2, s)
-- LEQ
bigStepB (Leq e1 e2,s) = let(n1,s1) = bigStepB(e1,s);
                                     (n2,s2) = bigStepB(e2,s)
                                     in(n1 <= n2, s)

-- Semântica de Comandos
bigStepC :: (C,Estado) -> (C,Estado)
-- SKIP                                     
bigStepC (SKIP,s) = (SKIP,s)
-- Atrib
bigStepC (Atrib (Var x) e1,s) = let(n1,s1) = bigStepE(e1,s)
                                        in (SKIP, mudaVar s x n1)
-- Seq
bigStepC (Seq c1 c2,s) = let (n1,s1) = bigStepC(c1,s)
                             in let (n2,s2) = bigStepC(c2,s1)
                             in (SKIP,s)
-- If
bigStepC (If b c1 c2,s) = let(b1,s1) = bigStepB(b,s)
                           in case b1 of 
                                True -> let(e1,s1) = bigStepC(c1,s)
                                                     in(SKIP,s1) 
                                False -> let(e2,s2) = bigStepC(c2,s)
                                                      in(SKIP,s2)                     

-- While
bigStepC (While b c,s) = let (b1,s1) = bigStepB(b,s)
                             in case b1 of 
                                True -> let(_,s2) = bigStepC(c,s)
                                           (_,s3) = bigStepC(While b c,s2)
                                           in(SKIP,s3) 
                                False -> (SKIP,s) 
