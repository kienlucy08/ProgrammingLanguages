-- Lucy Kien
-- Daily One Homework
-- Programming Languages 

module DailyOne where

    --Quadtraic function which takes in 4 params a,b,c and x and returns an integer
    quadratic :: Double -> Double -> Double -> Double -> Double
    quadratic a b c x = a + b*x + c*x^2

    --Scale Vector function that takes a single number and 2-tuple, returns a 2-tuple
    scaleVector :: Double -> (Double, Double) -> (Double, Double)
    scaleVector n (x,y) = (x*n, y*n)

    --Triple Distance that takes 2 3-tuples, returns the cartesian distance between them
    --Uses the distance formula
    tripleDistance :: (Double, Double, Double) -> (Double, Double, Double) -> Double
    tripleDistance (x,y,z) (i,j,k) = sqrt((i-x)^2+(j-y)^2+(k-z)^2)


