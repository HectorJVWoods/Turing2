module BaseFunctions(BaseFunction, allPossibleBaseFunctions, baseFunctionFromString, generateTruthTable) where
import Tape
import GatesFile
import Helpers (boolToChar, findBetween, isInt, charToInt)
import Data.Char (isDigit)

type BaseFunction = Bool -> Bool -> Bool
  
  
generateTruthTable :: BaseFunction -> String
generateTruthTable bf = line1 ++ "\n" ++ line2 ++ "\n" ++ line3 ++ "\n" ++ line4 ++ "\n" ++ line5
                   where line1     = "|A||B||C|"
                         line2     = "|0||0||" ++  [boolToChar a] ++  "|"
                         line3     = "|1||0||" ++  [boolToChar b] ++  "|"
                         line4     = "|0||1||" ++  [boolToChar c] ++  "|"
                         line5     = "|1||1||" ++  [boolToChar d] ++  "|"
                         (a,b,c,d) = generateTruthValues bf
                         generateTruthValues f = (f False False, f False True, f True False, f True True)  

baseFunctionFromTruthValues :: TruthValues -> BaseFunction
baseFunctionFromTruthValues (a,b,c,d) = f
                                   where f False False = a
                                         f False True  = b
                                         f True False  = c
                                         f True True   = d

baseFunctionFromString :: GatesList -> String -> BaseFunction
baseFunctionFromString gatesList s = baseFunctionFromTruthValues tValues
                                  where tValues | isAName   = getTruthValuesForName gatesList s
                                                | otherwise = (a,b,c,d)
                                                where isAName = null betweenBrackets
                                                      betweenBrackets = findBetween '(' ')' s
                                                      (a,b,c,d) | length betweenBrackets == 7 = (f aS, f bS, f cS,f dS)
                                                                | otherwise = error ("baseFunctionFromString: Too few/many truth values for (a,b,c,d) expression on line: " ++ s)
                                                      f x | isDigit x = charToBool x
                                                          | otherwise = error ("baseFunctionFromString: char is not a number for (a,b,c,d) expression on line: " ++ s)
                                                      (aS,bS,cS,dS) = (head betweenBrackets, betweenBrackets !! 2, betweenBrackets !! 4, betweenBrackets !! 6)




                                           
allPossibleBaseFunctions :: [BaseFunction]
allPossibleBaseFunctions = [baseFunctionFromTruthValues (a,b,c,d) | a <- [True, False], b <- [True, False], c <- [True, False], d <- [True, False]]






charToBool :: Char -> Bool
charToBool '1' = True
charToBool '0' = False
charToBool _   = error "Invalid character"