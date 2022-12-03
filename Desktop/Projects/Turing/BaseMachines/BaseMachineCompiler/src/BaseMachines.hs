module BaseMachines(BaseMachine, State, BaseFunction, TapePosition,
    newState, runState, runBaseMachine, stateToString, baseMachineToString) where


import Tape
import BaseFunctions
import GatesFile
import Helpers

type BaseMachine = [State]
type State = (BaseFunction, TapePosition, TapePosition)
type TapePosition = Int

baseMachineToString :: BaseMachine -> String
baseMachineToString [] = "!!!EMPTY BASE MACHINE!!!"
baseMachineToString (s:ss) = stateToString s ++ " \n => \n" ++ baseMachineToString ss

newStateFromString :: GatesList -> String -> TapePosition -> TapePosition -> State
newStateFromString gatesList s x y = (baseFunctionFromString gatesList s, x, y)

stateToString :: State -> String
stateToString (bf, x, y) = "{ \n" ++ generateTruthTable bf ++ " \n      A(" ++ show x ++ ") B(" ++ show y ++ ")}"

newState :: BaseFunction -> TapePosition -> TapePosition -> State
newState bf p1 p2 = (bf, p1, p2)

newTwoStateFromString :: GatesList -> String -> TapePosition -> TapePosition -> Int -> State
newTwoStateFromString gatesList s tp1 tp2 tapeLength = (baseFunctionFromString gatesList s, tp1, tp2 + tapeLength)

newTwoState :: BaseFunction -> TapePosition -> TapePosition -> Int -> State
newTwoState bf tape1Tp tape2Tp tapeLength = (bf, tape1Tp, tape2Tp + tapeLength)

runState :: State -> Tape -> TapeValue
runState (bf, p1, p2) t = bf (t !! p1) (t !! p2)


runBaseMachine :: BaseMachine -> Tape -> Tape
runBaseMachine [] _ = []
runBaseMachine (cs:states) t = runState cs t : runBaseMachine states (runState cs t : t)


