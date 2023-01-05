module Machine(Machine, NamedMachine, NamedMachineSet, MachineName, runMachineWithName) where
import SymbolsAndMappings
import MapSet

-- The most basic definition of a machine is something which takes a bunch of symbols, and converts them into a bunch of other symbols.
type Machine = ([Symbol] -> [Symbol])
-- How in fact a machine is implemented however is a more complex matter. There are more than likely an infinite number of
-- possible turing complete machines.
-- I don't see the point in reinventing the wheel though, so I'm just going to use haskell to interpret and run machines.
-- Eventually i plan on writing a lisp compiler as well since I think it's a bit closer to what I am trying to do
-- than haskell, but I'm not familiar with it yet so I'll stick with haskell for the time being.

type MachineName = String
type NamedMachine = Map MachineName Machine
type NamedMachineSet = MapSet MachineName Machine



getMachineByName :: MachineName -> NamedMachineSet -> Maybe Machine
getMachineByName = getBFromA

runMachine :: Expression -> Machine -> Expression
runMachine expression machine = machine expression

runMachineWithName :: NamedMachineSet -> Expression -> MachineName -> Expression
runMachineWithName namedMachines expression machineName = runMachine expression machine
                                                       where machine = f (getMachineByName machineName namedMachines)
                                                             f (Just x) = x
                                                             f Nothing = error ("No such machine " ++ machineName ++ " in named machine set.")

-- Subset machines
-- A subset machine is a machine which takes an expression of a given language, and returns another expression of
-- the same language. I call it a "subset machine" because the machine is not capable of producing expressions
-- that do not already exist in the original language. Subset machines essentially take an existing language and
-- produce a new one that is a subset of the original.
-- in a strict sense, all machines are subset machines, since they are all derived from the Symbol* language (the set of all
-- possible strings of symbols).
-- the haskell equivalent of a subset machine is a function of the type x :: a -> a



-- Translator machines/Mappings
-- A translator machine is a machine capable of converting an expression of one language into an expression of another language.
-- This means that a translator machine is capable of producing expressions that do not exist in the original language.
-- For example, let's consider the problem of converting a number into its spoken form, e.g the spoken form of 42.314 is
-- "forty two point three one four".
--
