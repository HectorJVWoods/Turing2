module MachineFileCompiler where

import Data.List (isPrefixOf)
import Machines
import Set
import Helpers (wordsWhen, splitBySublist)


-- Load file as string, then parse it
compileMachineFile :: String -> String -> Machine
compileMachineFile fileName fileContents = (macName, states)
                                    where
                                      states  = setFromList $ map parseState rest
                                      macName = head $ lines fileContents
                                      rest    = preParse $ unlines $ tail $ lines fileContents



-- Parse a single line into a state transition, which is a tuple of form
-- (leftSymbol, rightSymbol, nextState, direction)
-- in the file, the symbols are separated by "->", and the direction and next state
-- are written as ([stateName]/[direction])
parseState :: String -> StateTransition
parseState str = (leftSymbol, rightSymbol, nextState, direction)
              where
                   rightSymbol = symbolFromStr rsym
                   leftSymbol  = symbolFromStr lsym
                   (lsym, rsym) = splitBySublist symbols "->"
                   direction = directionFromString $ head $ tail $ tail splitByColon
                   nextState = head $ tail splitByColon
                   symbols = head splitByColon
                   splitByColon = wordsWhen (==':') str

-- Remove empty lines, remove lines that start with #, remove everything after a # on non-comment lines
-- using functions below
preParse :: String -> [String]
preParse = removeEmptyLines . removeCommentFromLine . removeCommentLines . removeWhitespace
        where removeWhitespace = filter (/= ' ')
              removeEmptyLines = filter (not . null) . lines
              removeCommentFromLine = unlines . map f . lines
                                   where f []     = []
                                         f (x:xs) | x == '#' = []
                                                  | otherwise = x : removeCommentFromLine xs
              removeCommentLines = unlines . filter isNotComment . lines
                  where
                      isNotComment [] = True
                      isNotComment (x:_) = x /= '#'






