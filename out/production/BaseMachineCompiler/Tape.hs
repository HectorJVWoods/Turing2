module Tape(Tape, TapeValue, TapeLength, tapeFromString, tapeToString) where
import Helpers
type TapeValue = Bool -- Single value on the tape
type TapeLength = Int
type Tape = [Bool]


tapeFromString :: String -> Tape
tapeFromString = map charToBool 

tapeToString :: Tape -> String
tapeToString = map boolToChar