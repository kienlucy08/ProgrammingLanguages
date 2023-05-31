--Lucy Kien
--Daily Ten
--Programming Languages

module DailyTen where

    import Data.Functor
    import Data.Eq

    --firstFunctorLaw that returns true if the first functor law holds for a functor value. 
    --Your function should have the type (Eq (f a), Functor f) => f a -> Bool. 
    --consumes a functor value of f a 
    --produces a boolean value
    firstFunctorLaw :: (Eq (f a), Functor f) => f a -> Bool
    firstFunctorLaw f = fmap id f == f

    --secondFunctorLaw that returns true if the second functor law holds for two functions and a functor that are passed to it.
    --Your function should have the type:  (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool.
    --consumes a functor value of f c and two functions (b->c) and (a->b)
    --produces a boolean value
    secondFunctorLaw :: (Eq (f c), Functor f) => (b -> c) -> (a -> b) -> f a -> Bool
    secondFunctorLaw f g x = (fmap f . fmap g) x == fmap (f . g) x