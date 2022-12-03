module BMIPF (stringToBMIPF, compileBaseMachine) where
import BaseMachines
import BaseFunctions
import GatesFile
import Helpers
import Data.Char (isDigit)
import Data.List (isInfixOf)

-- "Base Machine In Program Form"
data BaseMachineType = OneTape Int | TwoTape Int
instance Show BaseMachineType where
    show (OneTape n) = "OneTape (Length: " ++ show n ++ ")"
    show (TwoTape n) = "TwoTape (Length: " ++ show n ++ ")"
data BMIPF = Start BaseMachineType BMIPF | BMIPFState GateName TapePosition TapePosition BMIPF | End
instance Show BMIPF where
    show (Start bmType bmipf) = "Start " ++ show bmType ++ " " ++ show bmipf
    show (BMIPFState gateName tape1Pos tape2Pos bmipf) = "BMIPFState " ++ show gateName ++ " " ++ show tape1Pos ++ " " ++ show tape2Pos ++ " " ++ show bmipf
    show End = "End"


type GateName = String

convertTwoTapeToOneTape :: BMIPF -> BMIPF
convertTwoTapeToOneTape (Start (OneTape _) _) = error "convertTwoTapeToOneTape: BMIPF is already one tape"
convertTwoTapeToOneTape (Start (TwoTape tapeLength) bmipf) = Start (OneTape (tapeLength*2)) newStates
                                                          where newStates = f bmipf
                                                                f (BMIPFState gateName tape1Tp tape2Tp nextState) = BMIPFState gateName tape1Tp (tape2Tp + tapeLength) (f nextState)
                                                                f (Start _ _) = error "convertTwoTapeToOneTape: BMIPF has nested Start"
                                                                f End = End
convertTwoTapeToOneTape (BMIPFState _ _ _ _)  = error "convertTwoTapeToOneTape: Call this function on the top level BMIPF"
convertTwoTapeToOneTape End                   = End



compileBMIPF :: BMIPF -> GatesList -> BaseMachine
compileBMIPF (Start _ _) [] = error "compileBMIPF: Empty GatesList"
compileBMIPF BMIPFState {} [] = error "compileBMIPF: Empty GatesList"
compileBMIPF (Start (TwoTape tapeLength) bmipf) gs = compileBMIPF (convertTwoTapeToOneTape (Start (TwoTape tapeLength) bmipf)) gs
compileBMIPF (Start (OneTape tapeLength) bmipf) gs = f bmipf
      where f (BMIPFState gateName tape1Tp tape2Tp nextState) | tape1Tp > tapeLength = error "compileBMIPF: Tape 1 position is greater than tape length"
                                                              | tape2Tp > tapeLength = error "compileBMIPF: Tape 2 position is greater than tape length"
                                                              | otherwise            = (baseFunctionFromString gs gateName, tape1Tp, tape2Tp) : f nextState
            f (Start _ _) = error "compileBMIPF: BMIPF has nested Start"
            f End = []


stringToBMIPF :: String -> BMIPF
stringToBMIPF s = Start (tapeType tapeLength) states
                where
                      tapeType   | tTypeAsString == "(OneTape" && last tLengthAsString == ')' = OneTape
                                 | tTypeAsString == "(TwoTape" && last tLengthAsString == ')'= TwoTape
                                 | otherwise = error "stringToBMIPF: First line of BMIPF must be either (OneTape : <tapeLength>) or (TwoTape : <tapeLength>)"
                      tapeLength | not (null tLength) && isInt tLength = read tLength :: Int
                                 | otherwise = error "stringToBMIPF: Tape length must be an integer"
                      text    = removeWSpace s
                      tLength = filter isDigit tLengthAsString
                      (firstLine, restOfLines) = (head (lines text), tail (lines text))
                      (tTypeAsString, tLengthAsString) = split (removeComments firstLine) ':'
                      states = f restOfLines
                      removeComments [] = []
                      removeComments (a:as) | a == '#' = []
                                            | otherwise = a : removeComments as
                      f []        = End
                      f (line:xs) | null line      = f xs
                                  | isComment line = f xs
                                  | "OneTape" `isInfixOf` line || "TwoTape" `isInfixOf` line = error "stringToBMIPF: BMIPF has nested Start"
                                  | "END" `isInfixOf` line = parseEnd
                                  | otherwise              = BMIPFState gateName tape1AsInt tape2AsInt (f xs)
                                 where isComment x = head x == '#'
                                       parseEnd   | line == "(END)" = End
                                                  | otherwise       = error ("stringToBMIPF: Program must end with exactly '(END)' on line" ++ line)
                                       tape1AsInt | isInt tape1Tp = read tape1Tp :: Int
                                                  | otherwise     = error ("stringToBMIPF: Tape 1 position is missing or not an integer on line: " ++ line)
                                       tape2AsInt | isInt tape2Tp = read tape2Tp :: Int
                                                  | otherwise     = error ("stringToBMIPF: Tape 2 position is missing or not an integer on line: " ++ line)
                                       tape2Tp               = fst $ findBetweenAndReturnRest '[' ']' tape2Info
                                       (tape1Tp, tape2Info)  = findBetweenAndReturnRest '[' ']' tapeInfo
                                       (gateName, tapeInfo)  = findBetweenAndReturnRest '[' ']' commentsRemoved
                                       commentsRemoved = removeComments line

compileBaseMachine :: String -> GatesList -> BaseMachine
compileBaseMachine = compileBMIPF . stringToBMIPF