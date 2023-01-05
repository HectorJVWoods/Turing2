module SymbolsAndMappings(Expression, Symbol, SymbolSet, Mapping, getValueFromKey) where
import Set
-- All variables can be represented by a sequence of symbols, as can their data e.g
-- x = 1, y = "hello", z = 3
-- both x and 1 are symbols, "hello" is a sequence of symbols.
-- Symbols are the building blocks of types, and types are the building blocks of programs.
type Symbol = Char
type SymbolSet = Set Symbol -- A set of symbols
type Expression = [Symbol] -- An expression is a sequence of symbols


-- A mapping is a correspondence between two sets of symbols.
-- They are the building blocks of grammars (which generate languages) and machines (that perform computations).
-- See Grammars.hs and Machine.hs for more information on grammars and machines.

type Mapping = (Symbol, Symbol) -- A mapping is a correspondence between two expressions
type MappingSet = Set Mapping

getValueFromKey :: Symbol -> MappingSet -> Maybe Symbol
getValueFromKey key ms = case filter (\(k, _) -> k == key) (setToList ms) of
    [] -> Nothing
    (x:_) -> Just (snd x)