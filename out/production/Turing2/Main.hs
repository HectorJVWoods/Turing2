module Main(main) where

import System.IO
import Machines
import Set
import GHC.IO.Encoding
import MachineFileCompiler
import System.Directory (getDirectoryContents)
import Control.Monad (foldM)

exampleSet :: MachineSet
exampleSet = setFromList [replaceAll1sWith2sMachine, halt, overWriteMachine]

turingMachineTest :: Tape -> MachineName -> MachineSet -> IO()
turingMachineTest tape machine mSet = do
       putStrLn "-------- TURING MACHINE TEST BEGIN --------"
       putStrLn $ " Input: " ++ show tape
       putStrLn $ "Output: " ++ show (runSimulator machine mSet tape)
       putStrLn "-------- TURING MACHINE TEST END --------"

loadMachineFile :: FilePath -> IO Machine
loadMachineFile path = do
                  str <- readFile path
                  return $ compileMachineFile path str

filterMacPathsInDir :: FilePath -> IO [FilePath]
filterMacPathsInDir path = do
                    dirFilePaths <- getDirectoryContents path
                    macPaths <- foldM (\acc x -> if isMachineFile x then return $ x:acc else return acc) [] dirFilePaths
                    return $ map (path ++) macPaths

loadMachinesFromDir :: FilePath -> IO MachineSet
loadMachinesFromDir path = do
                  dirFilePaths <- filterMacPathsInDir path
                  machines <- mapM loadMachineFile dirFilePaths
                  return $ setFromList machines

rootMachineDir :: FilePath -- TODO: set this using a command line argument
rootMachineDir = "machines/"



-- Load a package, and append it to an existing set of machines
loadPackage :: MachineSet -> FilePath -> IO MachineSet
loadPackage oldSet path = do newSet <- loadMachinesFromDir $ rootMachineDir ++ path
                             return $ oldSet `setUnion` newSet

machineFileExtension :: String
machineFileExtension = ".machine"

isMachineFile :: FilePath -> Bool
isMachineFile path = dropWhile (/= '.') path == machineFileExtension



-- Test the loading of a machine from file
testCompileMachineFile :: FilePath -> IO()
testCompileMachineFile path = do
       str <- readFile path
       putStrLn "-------- MACHINE FILE COMPILER TEST BEGIN --------"
       putStrLn $ "    Input: (File at " ++ path ++ ")"
       putStrLn $ "Pre-parse: " ++ show (preParse str)
       putStrLn $ "   Output: " ++ show (compileMachineFile path str)
       putStrLn "-------- MACHINE FILE COMPILER TEST END --------"


packagesToLoad :: [FilePath] -- TODO: set this using a command line argument
packagesToLoad = ["examples/"]

main :: IO ()
main = do
        -- Initialize the machine set as an empty set
        let mSet = setFromList [] :: MachineSet
        -- Load the packages
        loadedSet <- foldM loadPackage mSet packagesToLoad
        print loadedSet
        -- Run code below
        turingMachineTest (tapeFromStr "1113333444") "1sWith2s" loadedSet

