module DeBruijinLambdaCalculus where

--             [1..]    (M N)                 (\M)
data LambdaE = N Int | App LambdaE LambdaE | Abs LambdaE deriving (Show, Eq)
-- N is a de Bruijin index, i.e. a number representing a variable
-- App is an application of two lambda expressions
-- Abs is an abstraction, i.e. a lambda expression
--https://jameshfisher.com/2018/03/15/a-lambda-calculus-interpreter-in-haskell/
eval :: LambdaE -> LambdaE
eval (App fun arg) = case eval fun of
  Abs body -> eval $ sub 0 body where
           sub n e = case e of
             App e1 e2 -> App (sub n e1) (sub n e2)
             Abs e'    -> Abs (sub (n+1) e')
             