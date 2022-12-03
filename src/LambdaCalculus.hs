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

data Lam =
    Var String -- x
    | App Lam Lam -- (x x)
    | Abs String Lam -- λx.x
    deriving (Show, Eq)

id = Abs "x" (Var "x")
true = Abs "t" (Abs "f" (Var "t"))
false = Abs "t" (Abs "f" (Var "f"))

substitution :: Lam -> String -> Lam -> Lam
substitution (Var y) x v   | x == y    = v
                           | otherwise = Var y
substitution (Abs y e) x v | x == y    = Abs y e
                           | otherwise =  Abs y (substitution e x v)
substitution (App e1 e2) x v = App (substitution e1 x v) (substitution e2 x v)

