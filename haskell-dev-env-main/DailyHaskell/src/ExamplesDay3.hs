module ExamplesDay3 where
    --first of a tuple
    --consume a two-tuple
    --produce a value of the first element of the tuple
    first :: (a,b) -> a
    first tup = fst(tup)