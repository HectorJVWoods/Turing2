module ECalc where

-- All programs compile down to this very simple language, loosely inspired by lambda calculus.

type MachineID = String
type MachineData = String
data EC = ECAbs MachineID EC | ECVar MachineData

-- 入機. roughly means "input machine" :), works well for an abstraction symbol don't you think?
-- or just 入 for short, since it's close to lambda.
-- In order to implement the calculus in your target language, the compiler need do only the following:
--   1. Hold a dictionary of type MachineID -> BaseMachine, where base machine is a function/gate in the target language
--   2. When it encounters an abstraction, check that the machineId is in the dictionary.
--      if not, throw an error.
--  3. When it encounters a variable, ignore it.