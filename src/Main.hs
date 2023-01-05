module Main(main) where

import System.IO
import Program

import System.Environment.Blank (getArgs)

filePath :: FilePath
filePath = "program.turing"
  
compileProgram :: IO()
compileProgram = do
       putStrLn "-------- COMPILATION BEGIN --------"
       args <- getArgs
       putStrLn $ "Arguments: " ++ show args
       progHandle <- openFile filePath ReadWriteMode
       progContents <- hGetContents progHandle
       print progContents
       return ()  
  
programTest :: Expression -> Program -> IO()
programTest expression program = do
       putStrLn "-------- PROGRAM TEST BEGIN --------"
       putStrLn $ "Input: " ++ show expression
       putStrLn $ "Program: " ++ show program
       putStrLn $ "Output: " ++ show (runProgram expression program)
       return ()


doubler :: Machine
doubler = [([Symbol '1'], [Symbol '2']), ([Symbol '2'], [Symbol '4'])]
  
main :: IO ()
main = compileProgram
