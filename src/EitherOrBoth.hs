module EitherOrBoth where
data EitherOrBoth a b = Left a | Right b | Both a b
data EitherBothOrNeither a b = A a | B b | AB a b | Neither