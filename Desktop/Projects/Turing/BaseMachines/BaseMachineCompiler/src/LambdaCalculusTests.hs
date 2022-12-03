module LambdaCalculusTests(lambdaTests) where
import LambdaCalculus




lambdaTests :: IO ()
lambdaTests = do
        putStrLn "-------- TESTS BEGIN --------"
        print fst'
        print fst'AppliedToTrue
        print $ betaReduce fst'AppliedToTrue
        putStrLn "-------- TESTS END --------"