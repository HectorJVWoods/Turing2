module Grammars(Grammar) where
import Set
import Helpers






-- A grammar is a set of production rules that generate a language.
-- For example the grammar for positive and negative integers is as follows:
-- S -> A  |  -A
-- A -> AA | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | ε
-- where ε is the empty string.
-- Symbols are either terminal, non-terminal, or epsilon.
-- A terminal symbol is a symbol that cannot be expanded any further.
-- A non-terminal symbol is a symbol that can be expanded into other symbols.
-- An expression is evaluated through derivation until there are no more non-terminal symbols.
data GrammarSymbol = Terminal Symbol | NonTerminal Symbol | Blank  deriving (Show, Eq)
type ProductionRule = ([GrammarSymbol], [GrammarSymbol]) -- A production rule is a mapping from one grammar symbol to another
type Grammar = (Set ProductionRule) -- A grammar is a set of production rules
data GrammarType = ContextFree | ContextSensitive
                     | Regular | RecursivelyEnumerable | Uncomputable deriving (Show, Eq) -- A grammar type is a type of grammar, e.g context free, context sensitive, regular, unrestricted


symbolToGrammarSymbol :: Symbol -> Set Symbol -> Symbol -> GrammarSymbol
symbolToGrammarSymbol s nonTerminals blankSymbol | s == blankSymbol       = Blank
                                                 | isInSet s nonTerminals = NonTerminal s
                                                 | otherwise              = Terminal s



newGrammarFromSymbols :: Set Symbol -> Symbol -> Set ([Symbol], [Symbol]) -> Grammar
newGrammarFromSymbols nonTerminals blankSymbol = setMap toGrammarSymbols
    where toGrammarSymbols (lhs, rhs) = (lhsGram, rhsGram)
              where lhsGram = map convert lhs
                    rhsGram = map convert rhs
                    convert s = symbolToGrammarSymbol s nonTerminals blankSymbol


stringToGrammar :: String -> Grammar
stringToGrammar s = newGrammarFromSymbols nonTerminals blankSymbol (setFromList (map toTuple (lines s)))
    where (startSymbol, nonTerminals, blankSymbol) = (firstLine, otherLines) = (head (lines s), tail (lines s))


intGrammar :: Grammar
intGrammar = newGrammarFromSymbols nonTerminals blank rules
          where nonTerminals = setFromList ['S', 'A']
                blank = 'ε'
                rules = setFromList [("S", "A"),
                                     ("S", "-A"),
                                     ("A", "AA"),
                                     ("A", "0"),
                                     ("A", "1"),
                                     ("A", "2"),
                                     ("A", "3"),
                                     ("A", "4"),
                                     ("A", "5"),
                                     ("A", "6"),
                                     ("A", "7"),
                                     ("A", "8"),
                                     ("A", "9"),
                                     ("A", "ε")]


grammarFromRules :: [ProductionRule] -> Grammar
grammarFromRules = setFromList

-- Let's implement the grammar for positive and negative integers
integerGrammar :: Grammar
integerGrammar = grammarFromRules rules
    where
        rules = [([NonTerminal 'S'], [NonTerminal 'A']),
                 ([NonTerminal 'S'], [Terminal '-', NonTerminal 'A']),
                 ([NonTerminal 'A'], [NonTerminal 'A', NonTerminal 'A']),
                 ([NonTerminal 'A'], [Terminal '0']),
                 ([NonTerminal 'A'], [Terminal '1']),
                 ([NonTerminal 'A'], [Terminal '2']),
                 ([NonTerminal 'A'], [Terminal '3']),
                 ([NonTerminal 'A'], [Terminal '4']),
                 ([NonTerminal 'A'], [Terminal '5']),
                 ([NonTerminal 'A'], [Terminal '6']),
                 ([NonTerminal 'A'], [Terminal '7']),
                 ([NonTerminal 'A'], [Terminal '8']),
                 ([NonTerminal 'A'], [Terminal '9']),
                 ([NonTerminal 'A'], [Blank])]




-- I broke this code by changing some types around, not important but I leave it since
-- Eventually will want to rewrite this code in Turing.
{-
-- Currently the grammar allows for blank symbols to be on the left hand side of a production rule. Such grammars
-- Are not computable on a turing machine, so we need a way to check if this is the case.
-- The set of grammars that do not allow for the left hand side of a production rule to be blank are called Type-0 grammars,
-- and generate Recursively enumerable languages.

isType0ProdRule :: ProductionRule -> Bool
isType0ProdRule (lhs, _) = lhs /= Blank

isType0 :: Grammar -> Bool
isType0 = all isType0ProdRule . listFromSet

intGrammarIsType0 :: Bool
intGrammarIsType0 = isType0 integerGrammar

-- Turing machines are great and all, but they have a bit of a problem when it comes to practical computation;
-- Turing machines are built with the assumption that one has infinite memory, which is not the case in the real world.
-- This is not incidental but a necessity; some of the languages generated by a turing machine do indeed require infinite memory.
-- However, if we want the set of languages that we can actually compute on a computer, we need to restrict the set of languages
-- to those that can be computed on a computer with finite memory.
-- These languages are known as context-sensitive languages, and are generated by Type-1 grammars.
-- Type 1 grammars introduce the following rules:
-- 1. The left hand side of a production rule must be aAb, where A is a non-terminal symbol, and a and b
-- are a sequence of terminal/non-terminal symbols. a and b can be empty, but A must be present.
-- 2. The right hand side of a production rule must be of the form ayb, where y is a sequence of terminal/non-terminal symbols,
-- and may not be empty, and a and b are sequences of terminal/non-terminal symbols, but may be empty.
-- 3. In addition type-1 grammars must also be type-0.
-- The crux of these rules is that the left hand side must consist of at least one non-terminal symbol
-- (and then any number of additional terminal/non-terminal symbols), and the right hand side must consist of at least one
-- symbol of any kind.

isType1 :: Grammar -> Bool
isType1 = all isType1ProdRule . listFromSet

isType1ProdRule :: ProductionRule -> Bool
isType1ProdRule (lhs, rhs) = isType1LHS lhs && isType1RHS rhs
                         where isType1LHS xs = case xs of
                                                [] -> False
                                                (NonTerminal _:_) -> True -- At least one non-terminal symbol, so it's type-1
                                                (Terminal _:ys)   -> isType1LHS ys -- If the first symbol is terminal, check the rest
                                                (Blank:zs)        -> isType1LHS zs -- Same for blank
                               isType1RHS xs = case xs of
                                                []      -> False -- Assumed to be equivalent to [Blank]
                                                [Blank] -> False -- Consists of only blank, so not type-1
                                                _       -> True -- Consists of at least one non-blank symbol, so type-1



intGrammarIsType1 :: Bool
intGrammarIsType1 = isType1 integerGrammar

-- We will now look at CFGs, or context-free grammars. The main advantage of CFGs is that we can compute them on an NPDA
-- Meaning that to recognize them we only need a stack, rather than a heap like with type 1 grammars.
-- CFGs are generated by type-2 grammars, which are defined as follows:
-- 1. The left side of a production rule must be a single non-terminal
-- 2. The right side of a production rule is a sequence of terminal/non-terminal symbols, and may also be empty.
-- The astute among you may notice that type 2 grammars allow for the empty string in the right hand side of a production rule
-- to be a blank symbol, but this is not the case for type 1 grammars. If type-2 grammars are a subset of type-1, how can this be the case?
-- https://stackoverflow.com/questions/39167387/can-a-context-sensitive-grammar-have-an-empty-string
-- This stack overflow explains it quite well ^ but basically the crux of it is that lambda productions are never necessary in type-2 grammars
-- and there is a fairly simple algorithm to eliminate them entirely. With this being the case, I will not allow lambda productions in my
-- implementation, in the hopes that this will encourage programmers to not include unnecessary lambda productions, and
-- to preserve the idea of type-1 grammars being a subset of type-2 grammars (which is not *technically* the case when lambda productions
-- are allowed).

isSingleNonTerminal :: [GrammarSymbol] -> Bool
isSingleNonTerminal xs = case xs of
                           [NonTerminal _] -> True
                           _              -> False

isType2ProdRule :: ProductionRule -> Bool
isType2ProdRule (lhs, rhs) = isSingleNonTerminal lhs && isType2RHS rhs
                         where isType2RHS [] = False
                               isType2RHS (NonTerminal _:ys) = isType2RHS ys
                               isType2RHS (Terminal _:zs) = isType2RHS zs
                               isType2RHS (Blank:_) = False -- lambda productions are not allowed, see above

isType2 :: Grammar -> Bool
isType2 = all isType2ProdRule . listFromSet

-- Finally regular languages, generated by type-3 grammars.
-- These languages are super easy to recognize. A finite state automata is capable of recognizing them, which is essentially
-- a turing machine with no capacity for memory. This does mean that regular languages are incredibly limited, but they are
-- very easy to compute.
-- Type-3 grammars are defined as follows:
-- 1. The left side of a production rule must be a single non-terminal symbol
-- 2. The right side of a production rule must be a single terminal symbol, or a terminal symbol followed by a single non-terminal symbol
-- There is another definition that allows for non-terminals to be before terminal symbols, but these offer no additional power
-- so I will not include them. In addition some definitions allow for lambda productions, but much like with type-2 grammars,
-- they do not offer any additional power, and are not necessary, so I will not include them.

isType3ProdRule :: ProductionRule -> Bool
isType3ProdRule (lhs, rhs) = isSingleNonTerminal lhs && isType3RHS rhs
                          where isType3RHS [] = False
                                isType3RHS [Terminal _] = True
                                isType3RHS [NonTerminal _, Terminal _] = True
                                isType3RHS _ = False


isType3 :: Grammar -> Bool
isType3 = all isType3ProdRule . listFromSet

-}