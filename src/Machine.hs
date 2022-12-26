module Machine where
import Grammars

-- The most basic definition of a machine is something which takes a bunch of symbols, and converts them into a bunch of other symbols.
type Machine = ([Symbol] -> [Symbol])
-- How in fact a machine is implemented however is a more complex matter. There are more than likely an infinite number of
-- possible turing complete machines.
-- I don't see the point in reinventing the wheel though, so I'm just going to use haskell to interpret and run machines.
-- Eventually i plan on writing a lisp compiler as well since I think it's a bit closer to what I am trying to do
-- than haskell, but I'm not familiar with it yet so I'll stick with haskell for the time being.



-- Subset machines
-- A subset machine is a machine which takes an expression of a given language, and returns another expression of
-- the same language. I call it a "subset machine" because the machine is not capable of producing expressions
-- that do not already exist in the original language. Subset machines es
-- For example, if we consider the language of integers, we could implement the (<) operator as a subset machine.
-- Such a machine takes a member of the integer language, and returns a 