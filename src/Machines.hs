module Machines where

import Set
type MachineName = String
data Symbol = Sym Char | Empty | Else | Skip | Separator deriving (Eq)
-- Empty is as you would expect, and there are an infinite number on either side of the tape.
-- Else is a wildcard symbol used on the left side of a transition rule, and matches any symbol.
-- The simulator should only consider Else rules if no other rules match.
-- Skip is a symbol used on the right side of a transition rule, and means that the symbol should not be changed.
-- Separator is a symbol used to separate multiple tapes on a single tape.
-- This is mostly just for convenience; you could simulate multi-tape machines without it by specifying
-- a separator symbol for each tape, but I think that approach is a bit more cumbersome, and having all machines
-- use the same separator symbol provides clarity.
-- "Else" and "Skip" should NEVER be part of a tape, and should only be used in transition rules.
-- It is okay for a tape to contain Separator symbols, and generally when showing an output tape, the simulator
-- should show each tape as a separate line, with the separator symbol removed.

instance Show Symbol where
   show (Sym c) = [c]
   show Empty = "{[Empty]}"
   show Else = "{[Else]}"
   show Skip = "{[Skip]}"
   show Separator = "{[Separator]}"
type Machine = (MachineName,Set StateTransition) -- A machine is a set of state transitions, i.e it is itself a state
data Direction = L | R | S deriving (Eq, Show)-- Left, Right, or Stay
type StateTransition = (Symbol, Symbol, MachineName, Direction) -- Replace a with b in the transition, and move to [MachineName]


type Tape = [Symbol]
type MacProduction = (Tape, MachineName, Tape) -- (Left of tape head, Current State, Right of tape head)

type MachineSet = Set Machine

tapeFromStr :: String -> [Symbol]
tapeFromStr = map Sym

symbolFromStr :: String -> Symbol
symbolFromStr "{[Empty]}" = Empty
symbolFromStr "{[Else]}" = Else
symbolFromStr "{[Skip]}" = Skip
symbolFromStr "{[Separator]}" = Separator
symbolFromStr c | length c == 1 = Sym $ head c
                | otherwise = error $ "Symbols should be of length 1 '" ++ c ++ "' is not."

directionFromString :: String -> Direction
directionFromString "L" = L
directionFromString "R" = R
directionFromString "S" = S
directionFromString c   = error $ "Invalid direction character '" ++ c ++ "'"

replaceAll1sWith2sMachine :: Machine
replaceAll1sWith2sMachine = ("1sWith2s", setFromList [(Sym '1', Sym '2', "1sWith2s", R),
                                                      (Empty, Empty, "1sWith2s", S),
                                                      (Else, Skip, "1sWith2s", R)])


-- This is the first instance of a two-tape machine.
-- It is a simple machine that overwrites the first tape with the second tape.
-- In reality there is only one tape, but the machine distinguishes between the two by the separator symbol.
-- It also removes the separator symbol, as we've reduced the two tapes to one.
overWriteMachine :: Machine
overWriteMachine = ("overWrite", setFromList [(Separator, Empty, "halt", S), -- If we see a separator, then we are done
                                              (Else, Empty, "overWrite", R)]) -- Otherwise, replace the symbol with an empty

multipleTapesFromStrs :: [String] -> Tape
multipleTapesFromStrs [] = []
multipleTapesFromStrs(x:xs) | null xs = tapeFromStr x
                            | otherwise = tapeFromStr x ++ [Separator] ++ multipleTapesFromStrs xs

removeEmpties :: Tape -> Tape
removeEmpties = filter (/= Empty)

halt :: Machine -- Equivalent to id
halt = ("halt", setFromList [(Else, Skip, "halt", S)])

idMachine :: Machine
idMachine = ("id", setFromList [(Else, Skip, "id", S)])

runSimulator :: MachineName -> MachineSet -> Tape -> Tape
runSimulator initState mSet iTape = left ++ right
   where
     (left,_,right) = runUntilHalt mSet (tapeToProd initState iTape)

tapeToProd :: MachineName ->  Tape -> MacProduction
tapeToProd initState tape = ([], initState, tape)

prodToTape :: MacProduction -> Tape
prodToTape (left,_,right) = left ++ right

-- When a machine is in the same state and the input does not change, we take that as halting.
runUntilHalt :: MachineSet -> MacProduction -> MacProduction
runUntilHalt mSet (left, stateName, right) | newStateName == stateName && left == newLeft && right == newRight = (left, stateName, right)
                                           | otherwise = runUntilHalt mSet (newLeft, newStateName, newRight)
   where
     newLeft = removeEmpties nL -- Remove empties from either side of the tape, since they do not hold any information
     newRight = removeEmpties nR
     (nL, newStateName, nR) = machineStep mSet (left, stateName, right)

machineStep :: MachineSet -> MacProduction -> MacProduction
machineStep mSet (left, stateName, right) = (newLeft, newStateName, newRight)
   where
     (newLeft,newRight) | direction == L = (init left, last left : newSymbol : right)
                        | direction == R = (left ++ [newSymbol], restOfTape)
                        | direction == S = (left, newSymbol : restOfTape)


     newSymbol = case nSymbol of
       Skip -> symbol -- If the new symbol is Skip, then don't change the symbol
       _ -> nSymbol

     (direction, newStateName, nSymbol) = case symbolSearch of
        Nothing -> case elseSearch of -- If not found, try for an Else transition
                           Nothing -> (S, stateName, symbol) -- No Else transition, so stay in the same state without changing the symbol
                           Just (_,b,m,d) -> (d, m, b)     -- No legal transitions, so stay in the same state without changing the symbol
                   where elseSearch = searchSet (\(a,_,_,_) -> a == Else) curTransitionRules
        Just (_,b,m,d) -> (d, m, b)
     symbolSearch = searchSet (\(a,_,_,_) -> a == symbol) curTransitionRules
     (symbol:restOfTape) | null right = [Empty] -- Lambda if right end of tape is empty
                         | otherwise = right
     curTransitionRules = case searchResult of
       Nothing -> error ("No such machine " ++ stateName ++ " in machine set.")
       Just (_,m) -> m
     searchResult = searchSet (\(m,_) -> m == stateName) mSet


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
