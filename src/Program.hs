module Program where
import Machine
import SymbolsAndMappings


type ProgramSequence = [MachineName]

-- A program consists of a sequence of names, each of which is a name of a machine and a set of named machines.
type Program = (ProgramSequence, NamedMachineSet)

runProgram :: Expression -> Program -> Expression
runProgram expression ([], _)                            = expression
runProgram expression (machineName:mns, namedMachines) = runProgram macResult (mns, namedMachines)
                                         where macResult = runMachineWithName namedMachines expression machineName
                                                                                            