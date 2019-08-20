module NumberClassifier
    ( 
    isFactor,
    allFactors,
    aliquotSum,
    numberClassifier,
    ) where

isFactor :: Int -> Int -> Bool
isFactor x y = x `mod` y == 0

allFactors :: Int -> [Int]
allFactors x = isFactor x `filter` [1 .. x]

aliquotSum :: Int -> Int
aliquotSum x = sum (allFactors x) - x

numberClassifier :: Int -> String
numberClassifier x
        | aliquotSum x > x = "This number is abundant"
        | aliquotSum x < x = "This number is deficient"
        | aliquotSum x == x = "This number is perfect!"
        | otherwise    = "This doesn't appear to be a number..."
