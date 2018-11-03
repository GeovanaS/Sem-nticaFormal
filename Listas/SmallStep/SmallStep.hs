import Estado

-- Expressões Aritméticas
data E = Num Int | Soma E E | Sub E E | Mult E E | Var String
     deriving(Eq,Show)

-- Expressões Booleanas
data B = TRUE | FALSE | Eq E E | Leq E E | Not B | And B B | Or B B
     deriving(Eq,Show)

-- Comandos
data C = SKIP | Atrib E E | Seq C C | If B C C | While B C
     deriving(Show)  

 -- Semântica de Expressões Aritmeticas    

smallStepE :: (E,Estado) -> (E,Estado)
 -- Var
smallStepE (Var x,s) = (Num(procuraVar s x),s) 
-- Soma3 
smallStepE (Soma (Num n1) (Num n2),s) = (Num(n1+n2),s)
-- Ex: smallStepE (Soma (Num 2)(Num 3),[("x",0)]) 

-- Soma2
smallStepE (Soma (Num n) e1,s) = let(e,s1) = smallStepE(e1,s)
                                             in(Soma(Num n) e,s)
-- Ex: smallStepE (Soma (Num 2)(Soma(Num 2)(Num 2)),[("x",0)])

-- Soma1
smallStepE(Soma e1 e2,s) = let(e,s1) = smallStepE(e1,s)
                                       in(Soma e e2,s)
-- Ex: smallStepE (Soma (Soma(Num 5)(Num 5))(Soma(Num 2)(Num 2)),[("x",0)])


-- Sub

-- Sub3 
smallStepE (Sub (Num n1) (Num n2),s) = (Num(n1-n2),s)

-- Sub2
smallStepE (Sub (Num n) e1,s) = let(e,s1) = smallStepE(e1,s)
                                            in(Sub(Num n) e,s)

-- Sub1
smallStepE(Sub e1 e2,s) = let(e,s1) = smallStepE(e1,s)
                                      in(Sub e e2,s)


-- Mult
smallStepE (Mult (Num n1) (Num n2),s) = (Num(n1-n2),s)

-- Sub2
smallStepE (Mult (Num n) e1,s) = let(e,s1) = smallStepE(e1,s)
                                            in(Mult(Num n) e,s)

-- Sub1
smallStepE(Mult e1 e2,s) = let(e,s1) = smallStepE(e1,s)
                                      in(Mult e e2,s)


-- Semântica de Expressões Booleanas

-- Not
smallStepB (Not FALSE,s) = (TRUE,s)
smallStepB (Not TRUE,s) = (FALSE,s)
smallStepB (Not b,s) = let(b1,s1) = smallStepB(b,s)
                                    in(Not b1,s1)
-- smallStepB (Not (And TRUE FALSE),[("x",0)])


-- And 

-- E3
smallStepB (And FALSE b,s) = (FALSE,s)
-- E2
smallStepB (And TRUE b,s) = (b,s)
-- E1
smallStepB (And b1 b2,s) = let(b,s1) = smallStepB(b1,s)
                                       in(And b b2,s1) 

-- Or 

-- OU3
smallStepB (Or FALSE b,s) = (b,s)
-- OU2
smallStepB (Or TRUE b,s) = (TRUE,s)
-- OU1
smallStepB (Or b1 b2,s) = let(b,s1) = smallStepB(b1,s)
                                       in(Or b b2,s1) 


-- Eq
smallStepB (Eq (Num n1) (Num n2),s) = if (n1==n2) then (TRUE,s) else (FALSE,s)                                     

smallStepB (Eq (Num n1) e2,s) = let(e,s1) = smallStepE(e2,s)
                                            in(Eq (Num n1) e,s1)

smallStepB (Eq e1 e2,s) = let(n1,s1) = smallStepE(e1,s);
                                       in(Eq n1 e2, s1)



-- Semântica de Comandos

-- Atrib

