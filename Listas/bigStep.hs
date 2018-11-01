data E = Num Int | Soma E E| Mult E E
    deriving(Eq,Show)

-- 3 * (5 + 10)
prog1::E
prog1 =  Mult(Num 3) (Soma(Num 5)(Num 10))


-- 10 * 5 + 2 * 12
prog2::E
prog2 = Soma (Mult(Num 10)(Num 5))
             (Mult (Num 2) (Num 12))


bigStep E:: E -> Int
bigStep E (Num n) = n
