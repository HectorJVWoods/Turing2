module LambdaCalculus(LCTerm, betaReduce, fst', fst'AppliedToTrue) where


-- True:
-- TRUE := λx.λy.x
-- False:
-- FALSE := λx.λy.y
-- NOT:
-- NOT := λp.p FALSE TRUE

-- Example of using NOT:
-- p := True = λx.λy.x
-- TRUE FALSE TRUE = λx.λy.x FALSE TRUE =

type Symbol = String
data LCTerm = Binding Symbol LCTerm | Application LCTerm LCTerm | Variable Symbol deriving (Eq)
instance Show LCTerm where
    show (Binding symbol term) = "\\" ++ symbol ++ "." ++ show term
    show (Application term1 term2) = "(" ++ show term1 ++ " " ++ show term2 ++ ")"
    show (Variable symbol) = symbol

-- substitution is defined as:
-- [a := b] i.e "replace a with b"
-- For Variables:
--x[ x := e ] = e (i.e replace instances of x with e)
--y[ x := e ] = y (i.e don't replace instances of non-x with e)
-- For bindings:
--(λx.e1)[ x := e2 ] = (λx.e1[ x := e2 ]) (i.e replace instances of x with e2 in e1)
-- For Applications:
--(e1 e2)[ x := e3 ] = (e1[ x := e3 ] e2[ x := e3 ]) (i.e replace instances of x with e3 in e1 and e2)
substitute :: LCTerm -> Symbol -> LCTerm -> LCTerm
substitute (Application e1 e2) y e3 = Application (substitute e1 y e3) (substitute e2 y e3)
substitute (Binding x e1) y e2      = Binding x (substitute e1 y e2)
substitute (Variable x) y e         | x == y    = e
                                    | otherwise = Variable x

betaReduce :: LCTerm -> LCTerm
betaReduce (Application (Binding x t1) t2) = substitute t1 x t2
betaReduce (Application t1 t2)             = Application (betaReduce t1) (betaReduce t2)
betaReduce (Binding x t)                   = Binding x (betaReduce t)
betaReduce t                               = t

fst' :: LCTerm
fst' = Binding "x" (Binding "y" (Variable "x"))

fst'AppliedToTrue :: LCTerm
fst'AppliedToTrue = Application fst' (Variable "True")
