module Problems where
import Grammars
import Set

type ProblemResult = [Symbol]
-- A "problem" can be considered as a "question" that takes an input, and produces one of n outputs.
-- It is possible for a problem to have an infinite number of outputs, as is the case with the problem of generating
-- all members of a non-regular language. The definition used here however, will be for problems framed by the user,
-- intended to be solved by a machine.
-- A problem can be considered as the set of possible outputs for that problem. The input is implicit, and is a set
-- of symbols. A machine that solves a problem will take a set of symbols as input, and produce one of the possible
-- problem results as output.
type Problem = Set ProblemResult