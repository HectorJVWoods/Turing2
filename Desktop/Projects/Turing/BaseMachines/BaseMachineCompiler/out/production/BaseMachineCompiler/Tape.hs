module Tape(Tape, TapeValue, TapeLength, tapeFromString) where
import Helpers
type TapeValue = Bool -- Single value on the tape
type TapeLength = Int
type Tape = [Bool]


tapeFromString :: String -> Tape
tapeFromString = map charToBool 

