module Machine where

import Tape
import TuringType
import BaseMachines

type MacConnection = (TuringType, TapeLength) -- The input and output of a machine
data NonBaseMachine = NonBaseMachine MacConnection Machine MacConnection
type Machine = Either BaseMachine NonBaseMachine
data MachineTree = MachineTree Machine MachineTree MachineTree | Leaf | Tape



isValidMachineTransition :: MacConnection -> MacConnection -> Bool
isValidMachineTransition = (==)