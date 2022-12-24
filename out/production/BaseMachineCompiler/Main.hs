module Main (main) where
import BaseMachines
import System.IO
import BaseFunctions
import GatesFile
import Tape
import BMIPF
import System.Environment.Blank (getArgs)
import LambdaCalculus
import LanguagesAndProblems

gatesFilePath :: String
gatesFilePath = "user_gates.gates"

baseProgramFilePath :: String
baseProgramFilePath = "program.tbm" --tbm stands for "turing base machine"

programInput :: Tape
programInput = tapeFromString "1010101010010100010101101010111101011001010101101111010111011010111"

-- TODO: make this a command line argument, and store the output in a file somehow
compileBaseProgram :: IO()
compileBaseProgram = do
               putStrLn "-------- BASE PROGRAM COMPILATION BEGIN --------"
               args <- getArgs
               putStrLn $ "Arguments: " ++ show args
               gatesHandle <- openFile gatesFilePath ReadWriteMode
               gatesFileContents <- hGetContents gatesHandle
               print gatesFileContents
               let gatesList = gateListFromString gatesFileContents
               hClose gatesHandle
               programHandle <- openFile baseProgramFilePath ReadWriteMode
               programFileContents <- hGetContents programHandle
               let baseMachine = compileBaseMachine programFileContents gatesList
               putStrLn $ baseMachineToString baseMachine
               putStrLn "--------BASE PROGRAM COMPILATION DONE--------"
               --_ <- getLine
               putStrLn $ tapeToString $ runBaseMachine baseMachine programInput
               return ()


main :: IO ()
main = runLam





resetDefaultGates :: IO ()
resetDefaultGates = writeFile gatesFilePath $ gateListToString defaultGatesList


fivePossibleBaseFunctions :: [BaseFunction]
fivePossibleBaseFunctions = take 5 allPossibleBaseFunctions