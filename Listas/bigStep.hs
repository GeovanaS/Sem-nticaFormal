data E = Num Int | Soma E E| Mult E E | IF B E E
    deriving(Eq,Show)

-- 3 * (5 + 10)
prog1::E
prog1 =  Mult(Num 3) (Soma(Num 5)(Num 10))


-- 10 * 5 + 2 * 12
prog2::E
prog2 = Soma (Mult(Num 10)(Num 5))
             (Mult (Num 2) (Num 12))


bigStepE:: E -> Int
bigStepE (Num n) = n
bigStepE (Soma e1 e2) = (bigStepE e1) + (bigStepE e2)
bigStepE (Mult e1 e2) = (bigStepE e1) * (bigStepE e2)

-- IF B E E
bigStepE (IF b e1 e2) 
         | bigStepB b = bigStepE e1
         | otherwise = bigStepE e2


data B = TRUE | FALSE | Not B| And B B | Or B B
     deriving(Eq,Show)

-- T ^ Not(T v F)

prog3:: B
prog3 = And TRUE (Not(Or TRUE FALSE))

-- Not(F ^ T) v Not(F v T)
prog4:: B
prog4 = Or (Not(And FALSE TRUE)) (Not(Or FALSE TRUE)) 


bigStepB :: B -> Bool
bigStepB TRUE = True
bigStepB FALSE = False
bigStepB (Not b)
    | bigStepB b = False
    | otherwise = True

-- bigStepB (Not b) = case bigStepB b of
--                   True -> False
--                   False -> True

bigStepB (And b1 b2)
        | bigStepB b1 = bigStepB b2
        | otherwise = False
