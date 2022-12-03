module Errors(nestedStartError) where

-- This module contains error messages for the compilation progress
-- I.e, the haskell code is working as intended, but the user that wrote the tbm program did something wrong.
-- So there are still some uses of "error x" in the code, but they are only used when there is a bug in this
-- compiler rather than the user's code.



type Result a = Either Error a
data Error = Error {errorName :: String, errorMessage :: String, errorCode :: Int, errorLine :: String}



nestedStartError :: String -> Error
nestedStartError = Error "Nested start" "You can't have a start gate inside another start gate" 1



