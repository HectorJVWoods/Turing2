module Types where

-- The crux of types is they are essentially just languages, or combinations of languages.
-- "Type checking" is just a matter of running that language's acceptor on the input.
-- Type checking is not mandatory in turing, but in my view it is usually advisable to use it.
-- Turing does not do any type-checking at runtime since all programs are simply the manipulation of symbols.
-- Instead, type checking is performed exclusively at compile time; the typechecker is itself a turing program that
-- evaluates turing programs and determines whether they are well-typed or not.