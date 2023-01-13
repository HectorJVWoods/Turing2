module Main(main) where

import System.IO


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
  


  
main :: IO ()
main = compileProgram
