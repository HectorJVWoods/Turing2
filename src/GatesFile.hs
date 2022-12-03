module GatesFile(TruthValues, GatesList, entryFromString, gateListToString, defaultGatesList, 
searchGatesList, getTruthValuesForName, gateListFromString) where

import Tape
import Helpers

-- For any 2-bit gate, there are 4 possible results (a,b,c,d). TruthValues encapsulates these results.
-- |A|B|C|
-- |0|0|a|
-- |1|0|b|
-- |0|1|c|
-- |1|1|d|
type TruthValues = (TapeValue, TapeValue, TapeValue, TapeValue)



type GatesList = [Entry]
data Entry = Entry {entryName :: String,  entryTruthValues :: TruthValues} deriving (Show, Eq)

searchGatesList :: GatesList -> String -> Maybe Entry
searchGatesList [] _ = Nothing
searchGatesList (x:xs) s = if entryName x == s then Just x else searchGatesList xs s



defaultGatesList :: GatesList
defaultGatesList = [andGate, orGate, xorGate, eqvGate, implGate, xnorGate, nandGate, norGate, nImplGate]

andGate :: Entry
andGate = Entry {entryName = "AND", entryTruthValues = (False, False, False, True)}

orGate :: Entry
orGate = Entry {entryName = "OR", entryTruthValues = (False, True, True, True)}

xorGate :: Entry
xorGate = Entry {entryName = "XOR", entryTruthValues = (False, True, True, False)}

eqvGate :: Entry
eqvGate = Entry {entryName = "EQV", entryTruthValues = (True, False, False, True)}

implGate :: Entry
implGate = Entry {entryName = "IMPLY", entryTruthValues = (True, True, False, True)}

nImplGate :: Entry
nImplGate = Entry {entryName = "NIMPLY", entryTruthValues = (False, False, True, False)}

xnorGate :: Entry
xnorGate = Entry {entryName = "XNOR", entryTruthValues = (True, False, False, True)}

nandGate :: Entry
nandGate = Entry {entryName = "NAND", entryTruthValues = (True, True, True, False)}

norGate :: Entry
norGate = Entry {entryName = "NOR", entryTruthValues = (True, False, False, False)}







gateListToString :: GatesList -> String
gateListToString [] = ""
gateListToString (e:es) = entryToString e ++ "\n" ++ gateListToString es

gateListFromString :: String -> GatesList
gateListFromString s = map entryFromString $ lines s

getTruthValuesForName :: GatesList -> String -> TruthValues
getTruthValuesForName [] name     = error $ "No gate entry found for '" ++ name ++ "'"
getTruthValuesForName (e:es) name | name == entryName e = entryTruthValues e
                                  | otherwise = getTruthValuesForName es name



entryToString :: Entry -> String
entryToString e = "[" ++ eName ++ ":" ++ tValuesAsString ++ "]"
              where eName = entryName e
                    tValues = entryTruthValues e
                    tValuesAsString = truthValuesToString tValues


entryFromString :: String -> Entry
entryFromString s             = Entry {entryName = eName, entryTruthValues = tValues}
                where eName   = drop 1 $ takeWhile (/= ':') s
                      tValues = truthValuesFromString $ drop 1 $ dropWhile (/= ':') s


truthValuesToString :: TruthValues -> String
truthValuesToString (a,b,c,d) = "(" ++ [boolToChar a] ++ "," ++ [boolToChar b] ++ "," ++ [boolToChar c] ++ "," ++ [boolToChar d] ++ ")"

-- String of the form (a,b,c,d)
truthValuesFromString :: String -> TruthValues
truthValuesFromString s = (a,b,c,d)
                      where a = charToBool $ s !! 1
                            b = charToBool $ s !! 3
                            c = charToBool $ s !! 5
                            d = charToBool $ s !! 7



