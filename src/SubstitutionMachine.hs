module SubstitutionMachine(runSubstitutionMachine, substitutionMachineToMachine) where
import Set
import SymbolsAndMappings
import Machine

-- Very simple machine that for a given expression and set of mappings, apply all mappings to the expression and return the result.
-- Computation can be done by continuously applying the same machine or by applying a sequence of machines.
type SubstitutionMachine = Set Mapping

runSubstitutionMachine :: SubstitutionMachine -> Expression -> Expression
runSubstitutionMachine sm (x:xs) = case getValueFromKey x sm of
    Nothing -> x : runSubstitutionMachine sm xs
    Just y -> y : runSubstitutionMachine sm xs


swapZeroAndOneMachine :: SubstitutionMachine
swapZeroAndOneMachine = setFromList [('0', '1'), ('1', '0')]

substitutionMachineToMachine :: SubstitutionMachine -> Machine
substitutionMachineToMachine = runSubstitutionMachine -- Partial application,returns a function of type [Symbol] -> [Symbol]