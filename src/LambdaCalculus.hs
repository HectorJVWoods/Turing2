module LambdaCalculus(LambdaE, runLam) where

-- True:
-- TRUE := λx.λy.x
-- False:
-- FALSE := λx.λy.y
-- NOT:
-- NOT := λp.p FALSE TRUE

-- Example of using NOT:
-- p := True = λx.λy.x
-- TRUE FALSE TRUE = λx.λy.x FALSE TRUE =
-- https://www.youtube.com/watch?v=knD_5pBCmuI

--                x               (x x)           \x.x
data LambdaE = Var String | Abs String LambdaE | App LambdaE LambdaE deriving (Show, Eq)

betaReductionStep :: LambdaE -> LambdaE
betaReductionStep (App (Abs x t) s) = substitute t (Var x) s
betaReductionStep (App t s) = App (betaReductionStep t) s
betaReductionStep (Abs x t) = Abs x (betaReductionStep t)
betaReductionStep (Var x) = Var x

betaReduction :: LambdaE -> LambdaE -- Performs beta reduction until no more reductions can be made
betaReduction x = if x == betaReductionStep x then x else betaReduction (betaReductionStep x)


freeVariables :: LambdaE -> [LambdaE]
freeVariables (Var x) = [Var x] -- free variables of x are just x
freeVariables (Abs x e) = filter (/= Var x) (freeVariables e) -- free variables of \x.e are free variables of e, except x
freeVariables (App e1 e2) = freeVariables e1 ++ freeVariables e2 -- free variables of e1 e2 are free variables of e1 and free variables of e2


substitute :: LambdaE -> LambdaE -> LambdaE -> LambdaE
substitute term varName replacement = f term varName replacement
    where
        f (Var y) (Var x) r | x == y       = r -- y[x := r] = if x == y then r else y
                            | otherwise    = Var y
        f (App t s) (Var x) r = App (f t (Var x) r) (f s (Var x) r) -- (t s)[x := r] = (t[x := r] s[x := r])
        f (Abs y t) (Var x) r | x == y                       = Abs y t -- this would be redundant, so skip it
                              | Var y `elem` freeVariables r = error ("substitute: cannot substitute because of variable capture. Var " ++ y ++ " is free in " ++ show r)
                              | x /= y                       = Abs y (f t (Var x) r) -- \y.t[x := r]



alphaConvert :: LambdaE -> String -> LambdaE
alphaConvert (Var x) y = Var y
alphaConvert (App t s) y = App (alphaConvert t y) (alphaConvert s y)
alphaConvert (Abs x t) y = Abs y (substitute t (Var x) (Var y))






runLam :: IO ()
runLam = do
    runSim example1


runSim :: LambdaE -> IO ()
runSim e = do
    print e
    print $ betaReduction e
    print "------------------"

example1 :: LambdaE
example1 = App (Abs "x" (Abs "y" (Var "x"))) (Abs "x" (Abs "y" (Var "y")))
