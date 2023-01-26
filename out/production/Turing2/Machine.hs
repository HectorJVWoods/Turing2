module Program where

-- A program is just a combination of machines
-- i.e, machines are able to transition to other machines, but it is not possible
-- to run a machine, wait for it to halt, and then run another machine.
-- that is the job of a machine.

-- Machine files are what the user will write, machines are written as code blocks
-- The compiler turns machine files into two different files: one is machine files, one file for each machine.
-- The other is a program-flow file, which is a file that contains the flow of the program.
-- i.e the list of machines that the program will run in order.

-- The advantage of this approach is that if two programmers happen to define identical machines,
-- they will correspond to a single machine file.
-- likewise, if a machine has the same flow as another machine, the flow file will be identical.