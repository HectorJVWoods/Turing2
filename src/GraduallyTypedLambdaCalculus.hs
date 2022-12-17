module GraduallyTypedLambdaCalculus() where



type Variable = String
type BaseConstant = Variable -- Constant of name x
type BaseType = (String, [BaseConstant]) -- Base type of name x, with constants y1..yn


data LambdaType = Arrow LambdaType LambdaType | BType BaseType |Gradual  -- Types are either a base type or an arrow between a type and another type,
                                                                        -- i.e all types are constructed from base types
                                                                        -- or 'gradual' meaning their type is unmarked, and therefore not checked.

data TypedLambdaE = Var Variable  -- Variable x
                  | Abs Variable LambdaType TypedLambdaE -- Abstraction \x:T.e, where x is a variable, T is a type and e another expression
                  | App TypedLambdaE TypedLambdaE -- Application of two lambda expressions
                  | Const BaseConstant -- Base constant of type x named y


isConsistent :: LambdaType -> LambdaType -> Bool
isConsistent (BType (x, _)) (BType (y, _)) = x == y -- Two base types are consistent if they have the same name
isConsistent Gradual Gradual               = True -- Two unmarked types are consistent
isConsistent (Arrow t1 t2) (Arrow t3 t4)   = isConsistent t1 t3 && isConsistent t2 t4 -- Two arrow types are consistent if their arguments and return types are consistent
isConsistent _ _                           = False -- Otherwise they are not consistent


typeCheck :: [BaseType] -> TypedLambdaE -> TypedLambdaE
typeCheck validBaseTypes (Var x) = Var x -- Variables are always well typed

