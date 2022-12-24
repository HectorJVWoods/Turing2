module ECalc where

-- All programs compile down to this very simple language, loosely inspired by lambda calculus.

type MachineID = String
type MachineData = String
data EC = ECAbs MachineID EC | ECVar MachineData


-- Expression consist of:
-- Abstractions - Identical to simply typed lambda calculus abstractions. i.e:
-- (\x:T.e), where e is an expression and T is a type.
-- Named Abstractions - Named, non-anonymous, unlike lambda calculus, of the form:
-- (入x:T/"n".e) where e is an expression, T is a type, and n is a name.
-- e.g an abstraction that adds 1 to its input:
-- (入x:int/"add1".x+1)
-- Applications - Identical to simply typed lambda calculus applications. i.e:
-- (e1 e2), where e1 and e2 are expressions.


-- A compiler is a program that takes ECalc expressions and compiles them into the target language.
-- It also takes two sets as input:
-- 1. A set of named (non-anonymous) base machines that are available to the compiler,
-- This set
-- 2. A set of base types that are available to the compiler. It is perfectly fine for these types to not be
-- atomic, it is ultimately up to the compiler to decide how to handle them.
-- The type of a compiler is therefore ((ECalcProgram -> Set<Machine> -> Set<Type>) -> TargetLanguageProgram)
-- An "ECalcProgram" is simply an ordered list of ECalc expressions.
-- The structure of a TargetLanguageProgram of course depends on the language. The compiler can choose to evaluate
-- expressions in any order it wants, and have the output in any structure it so wishes.
-- The only constant with a compiler is that it takes the same inputs.
-- For example, a very simple compiler could be one to convert an ECalcProgram to a string. In this case the
-- target "Program" is simply a string.
-- A compiler should implement the following loop to compile an ECalcProgram:
-- 1. Take the next expression from the ECalcProgram
-- 2. If




-- 入機. roughly means "input machine" :), works well for an abstraction symbol don't you think?
-- or just 入 for short, since it's close to lambda.




type Variable = String
type BaseConstant = Variable -- Constant of name x
type BaseType = (String, [BaseConstant]) -- Base type of name x, with constants y1..yn


data LambdaType = Arrow LambdaType LambdaType | BType BaseType |Gradual  -- Types are either a base type or an arrow between a type and another type,
                                                                        -- i.e all types are constructed from base types
                                                                        -- or 'gradual' meaning their type is unmarked, and therefore not checked.
                                                                        
                                                                        
                                                                        
data ECalc = Var Variable
           | Const BaseConstant
           | Abs Variable LambdaType ECalc                                                                    