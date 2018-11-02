import Estado

-- Expressões Aritméticas
data E = Num Int | Soma E E | Sub E E | Mult E E | Var String
     deriving(Eq,Show)

-- Expressões Booleanas
data B = TRUE | FALSE | Eq B B | Leq B B | Not B | And B B | Or B B
     deriving(Eq,Show)

-- Comandos
data C = SKIP | Atrib Int E | Seq C C | If B C C | While B C
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

