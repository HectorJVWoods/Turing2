module Machine where
import Grammars






-- The most basic definition of a machine is something which takes a bunch of symbols, and converts them into a bunch of other symbols.
type Machine = ([Symbol] -> [Symbol])
