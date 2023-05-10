--Lucy Kien
--Weekly Assignment Three: Typeclasses
--Programming Languages

module WeeklyAssignmentThree where

    --Define a new type called Vec that contains a list of Double values. 
    --Use the constructor name Vec for it 
    data Vec = Vec [Double] deriving (Show)

    --Instantiate Vec as a member of Num and define the appropriate functions. 
    --Adding, subtracting and multiplying, the first elements are combined, followed by the second, and so forth.
    --Hint: use zipWith to solve the math functions in one line. 
    --fromInteger may be confusing: repeat to create the Vec
    instance Num Vec where
        (+) (Vec a) (Vec b) = Vec (zipWith (+) a b)
        (-) (Vec a) (Vec b) = Vec (zipWith (-) a b)
        (*) (Vec a) (Vec b) = Vec (zipWith (*) a b)
        negate (Vec a) = Vec (map negate a)
        abs (Vec a) = Vec (map abs a)
        signum (Vec a) = Vec (map signum a)
        fromInteger n = Vec (repeat (fromInteger n))

    --Instantiate Vec as a member of Eq and define the appropriate functions. 
    --Hint: use and and zipWith to solve this in one line. The and might be confusing, so try: "and [True, False, True]" and see what you get. 
    instance Eq Vec where
        (==) (Vec a) (Vec b) = and (zipWith (==) a b)

    --Instantiate Ord typeclass and define the appropriate functions.
    --Compare function which compares two vector sums
    instance Ord Vec where
        compare (Vec a) (Vec b) = compare (sum a) (sum b)
    
    --Create a new typeclass called VecT, this should have one function: 
    class VecT a where

        --magnitude :: VecT a => a -> Double.
        --Consumes a variables
        --Produces the magnitude of that variable
        magnitude :: a -> Double

    --Instantiate Vec as a VecT and define magnitude for your vector.
    --Magnitude vector equation sqrt sum a^2 with b^2
    instance VecT Vec where
        magnitude (Vec a) = sqrt (sum (map (^2) a))
        
    --Instantiate Vec as a Semigroup. We can use addition as the associative operator.
    --Zips the two vectors together and uses <> for vectors
    instance Semigroup Vec where
        (<>) (Vec a) (Vec b) = Vec (zipWith (+) a b)

    --Instantiate Vec as a Monoid. Hint: the identity value will have used repeat to be created. 
    instance Monoid Vec where
        mempty = Vec $ repeat 0

    







    




