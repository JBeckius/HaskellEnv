module Lib
    ( 
    someFunc,
    -- factorRange,
    head',
    listComp,
    fib,
    maximum',
    firstTwo,
    replicate',
    take',
    reverse',
    zip',
    elem',
    quicksort',
    map',
    plusThreeAll,
    zipWith',
    createTuple,
    filter',
    largestDivisible,
    oddSquares,
    collatzSeq,
    collatzCount,
    map'',
    map''',
    elem'',
    reverse'',
    --foldr1',
    product',
    filter'',
    last',
    lastN,
    and',
    sumSqr,
    last'',
    contains,
    uniqueEls,
    firstWord,
    countWords,
    -- listContainsList,
    encodeCaesarCipher,
    decodeCaesarCipher,
    sumDigits,
    firstTo,
    findKey,
    findKeyFold,
    phoneBook,
    insert',
    size,
    cleanPhoneNum,
    findOdd,
    factorial,
    rowSumOddNumbers,
    rowSumOddNumbers',
    highAndLow,
    isTriangle,
    humanReadable,
    splitsies,
    automorphic,
    breakByRuns,
    listToString,
    wave,
    uncertainty,
    getPINs,
    lcs,
    lexiPos,
    applyMaybe,
    length',
    mean,
    palindrome,
    isPalindrome,
    lengthSort,
    direction,
    directions,
    pointCompare,
    polarCompare,
    sortPolar,
    foldPoints,
    grahamScan,
    grahamScanDebug
    ) where

import Data.List
import Data.Char
import Data.Function ( on )
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

phoneBook :: Map.Map String String
phoneBook = Map.fromList $
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]


someFunc :: IO ()
someFunc = putStrLn "someFunc"

head' :: [a] -> a
head' [] = error "This function does not work on an empty list"
head' (x:_) = x

listComp :: [Float] -> [Float]
listComp [] = error "This function does not work on an empty list"
listComp xs = [gt2 | a <- xs, let gt2 = a * 2, gt2 > 2]

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = fib(x - 1) + fib(x - 2)

-- take a list of orderable items and return the largest item
maximum' :: Ord a => [a] -> a
maximum' [] = error "No maximum of an empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs) 

firstTwo :: [a] -> [a]
firstTwo [] = error "Can't call firstTwo on an empty list"
firstTwo [x] = error "Can't call firstTwo on a list of length 1"
firstTwo (x:y:_) = [x, y]

replicate' :: Int -> a -> [a]
replicate' n x
        | n <= 0 = []
        | otherwise = x : replicate' (n-1) x

-- This function returns a specified number of elements from a specified list. For instance, take 3 [5,4,3,2,1] will return [5,4,3].
take' :: Int -> [a] -> [a]
take' n xs
        | n <= 0 = []
take' _ []       = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (a:as) (b:bs) = (a,b) : zip' as bs

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
        | a == x    = True
        | otherwise = a `elem'` xs

-- quicksort takes a list and sorts it
-- It takes the first element of the list and 
-- splits the remaining els based on comparison
-- [< x] ++ [x] ++ [> x]
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
        let smallerOrEqual = filter (<= x) xs
            larger         = filter (> x) xs
        in quicksort' smallerOrEqual ++ [x] ++ quicksort' larger

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

plusThreeAll :: (Num a) => [a] -> [a]
plusThreeAll [] = []
plusThreeAll xs = map' (+3) xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

createTuple :: a -> b -> (a,b)
createTuple x y = (x, y)

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs


-- let’s find the largest number under 100,000 that’s divisible by 3,829
largestDivisible :: Integer
largestDivisible = head ( filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0


-- find the sum of all odd squares smaller than 10000
oddSquares :: Integer
oddSquares = sum (filter p [1..])
    where p x = x^2 < 10000 && x^2 `mod` 2 > 0

collatzSeq :: Integer -> [Integer]
collatzSeq 1 = [1]
collatzSeq x
    | even x = x : collatzSeq(x `div` 2)
    | odd x = x : collatzSeq(x*3 + 1)

-- For all starting numbers between 1 and 100, 
-- how many Collatz chains have a length greater than 15?
collatzCount :: Int
collatzCount = length (filter isLong (collatzSeq `map` [1..100]))
    where isLong xs = length xs > 15

-- map implementd with right fold
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- map implemented with left fold
-- the ++ function is much slower that the : syntax,
-- so the foldr implementation is preferred over foldl
map''' :: (a -> b) -> [a] -> [b]
map''' f xs = foldl (\acc x -> acc ++ [f x]) [] xs

-- checks whether value is part of a list
-- implemented with foldr
elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldr (\x acc -> if x == y then True else acc) False ys

-- foldl (\acc x -> acc ++ [x]) [] xs
-- foldr (\x acc -> x : acc) [] xs

-- foldr1' :: (Foldable t) => (a -> a -> a) -> t a -> a
-- foldr1' f xs = foldr f (head xs) (tail xs)

reverse'' :: [a] -> [a]
reverse'' = foldl (flip(:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr (\x acc -> if f x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

lastN :: Int -> [a] -> [a]
lastN 0 _ = []
lastN _ [] = []
lastN n xs = last' xs : lastN (n - 1) (take (length xs - 1) xs)

and' :: [Bool] -> Bool
and' = foldr (&&) True

-- How many elements does it take for the sum of the square roots of all natural numbers to exceed 1,000?
-- 1. Get the sqrt of all natural numbers
-- 2. Get the sum of each growing set of sqrts
-- 3. Go through the scan list, keeping each entry until the sum is greater than 1000
-- 4. Get the length of the list of sums
-- 5. Add 1 to determine number required to exceed

sumSqr :: Int
sumSqr =  length (takeWhile (<1000) $ scanl1 (+) $ map sqrt [1..]) + 1

last'' :: [a] -> a
last'' [x] = x
last'' (x:xs) = last'' xs

contains :: (Eq a) => a -> [a] -> Bool
contains _ [] = False
contains x (y:ys) =  x==y || contains x ys

uniqueEls :: (Eq a) => [a] -> Int
uniqueEls = length . foldl (\acc x -> if contains x acc then acc else x : acc) []

firstWord :: String -> String
firstWord = takeWhile (/= ' ')

-- Take a string
-- if !" ", takeWhile char isn't " "
-- if " ", takeWhile char is " "
-- words :: String -> [a]
-- words = 

countWords :: String -> [(String, Int)]
countWords = map (\xs -> (head xs, length xs)) . group . sort . words

-- make a function that takes two lists and tells us if the first list is wholly contained anywhere in the second list.
-- listContainsList :: (Eq a) => Bool -> [a] -> [a] -> Bool
-- listContainsList _ [] = True
-- listContainsList [] [x] = False
-- listContainsList (x:xs) (y:ys)
--     | y==x = listContainsList xs ys
--     | otherwise = listContainsList xs (y:ys)

encodeCaesarCipher :: Int -> String -> String
encodeCaesarCipher shift = map (chr . (+shift) . ord)

decodeCaesarCipher :: Int -> String -> String
decodeCaesarCipher shift = encodeCaesarCipher (negate shift)

sumDigits :: Int -> Int
sumDigits = sum . map digitToInt . show

firstTo :: Int -> Maybe Int
firstTo x = find (\y -> sumDigits y == x) [1..]

findKey :: (Eq a) => a -> [(a,b)] -> Maybe b
findKey key [] = Nothing
findKey key ((k,v):xs)
    | key == k = Just v
    | otherwise = findKey key xs

findKeyFold :: (Eq a) => a -> [(a, b)] -> Maybe b
findKeyFold key = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing

insert' :: (Ord k) => k -> a -> Map.Map k a-> Map.Map k a
insert' = Map.insert

size :: Map.Map k a -> Int
size = Map.size

cleanPhoneNum :: String -> [Int]
cleanPhoneNum = map digitToInt . filter' isDigit

-- Given an array, find the int that appears an odd number of times.

-- There will always be only one integer that appears an odd number of times.
findOdd :: [Int] -> Int
findOdd = fst . head . filter (\(x, y) -> y `mod` 2 /= 0) . map (\(x:xs) -> (x, 1 + length xs)) . group . sort

-- Given the triangle of consecutive odd numbers:

--              1
--           3     5
--        7     9    11
--    13    15    17    19
-- 21    23    25    27    29
-- ...
-- Calculate the row sums of this triangle from the row index (starting at index 1) e.g.:

-- rowSumOddNumbers 1 -- 1
-- rowSumOddNumbers 2 -- 3 + 5 = 8
factorial :: Int -> Int
factorial 0 = 0
factorial 1 = 1
factorial n = n + factorial (n - 1)

rowSumOddNumbers :: Int -> Int
rowSumOddNumbers n = sum . map (+ sum [1..n-1] * 2) . take' n $ filter odd [1..]

rowSumOddNumbers' :: Integer -> Integer
rowSumOddNumbers' n = n * (1 + n * (n - 1)) + n * (n - 1)

highAndLow :: String -> String
-- highAndLow = (\xs -> head xs ++ " " ++ last xs) . sort . map read . words
highAndLow =  (\xs -> show (maximum xs) ++ " " ++ show (minimum xs)) . map convertString . words

highAndLow' :: String -> String
highAndLow' xs = show (maximum ns) ++ " " ++ show (minimum ns)
  where ns = (map read $ words xs) :: [Int]

convertString :: String -> Int
convertString (x:xs)
    | x == '-' = negate $ read xs
    | otherwise = read (x : xs)

isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c =  (2 * longest) < sum [a, b, c]
    where longest = maximum [a, b, c]

isTriangle' :: Int -> Int -> Int -> Bool
isTriangle' a b c =
  case sort [a,b,c] of
     [min, middle, max] -> (min + middle) > max


-- Write a function, which takes a non-negative integer (seconds) as input and returns the time in a human-readable format (HH:MM:SS)

-- HH = hours, padded to 2 digits, range: 00 - 99
-- MM = minutes, padded to 2 digits, range: 00 - 59
-- SS = seconds, padded to 2 digits, range: 00 - 59
-- The maximum time never exceeds 359999 (99:59:59)

humanReadable :: Int -> String
humanReadable s = intercalate ":" $ map (pad . show) [hrs, mins, secs]
    where hrs = s `div` 3600
          mins = (s `mod` 3600) `div` 60
          secs = (s `mod` 3600) `mod` 60
          pad x = if length x < 2 then "0" ++ x else x

-- humanReadable' :: Int -> String
-- humanReadable' x = printf "%02d:%02d:%02d" h m s
--     where (y, s) = x `divMod` 60
--           (h, m) = y `divMod` 60

splitsies :: [a] -> ([a], a, [a])
splitsies xs@(x:xs') = (xs, x, xs')

difference :: Eq a => [a] -> [a] -> [a]
difference a b = filter (`notElem` b) a

-- groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
-- groupBy _ [] = []
-- groupBy eq (x:xs) = (x:ys) : groupBy eq zs
--                     where (ys, zs) = span (eq x) xs

-- groupByFirstLetter :: [a] -> [[a]]
-- groupByFirstLetter = groupBy (\x y -> head x == head y)

-- span :: (a -> Bool) -> [a] -> ([a],[a])
-- span _ xs@[] = (xs, xs)
-- span p xs@(x:xs')
--     | p x = let (ys, zs) = span p xs' in (x:ys,zs)
--     | otherwise = ([],xs)

automorphic :: Integer -> String
automorphic x =
  let reverseStrX = reverse $ show x
      reverseStrXX = reverse . show $ (x^2)
  in if reverseStrX == take (length reverseStrX) reverseStrXX then "Automorphic" else "Not!!"

breakByRuns :: [Int] -> [[Int]]
breakByRuns xs = map reverse . tail . reverse . foldr (\x ys@(y:ys') -> if succ (head y) == x then (x:y):ys' else [x]:ys) [[last xs]] $ reverse xs

listToString :: [Int] -> String
listToString xs = 
    let breakByRuns xs = map reverse . tail . reverse . foldr (\x ys@(y:ys') -> if succ (head y) == x then (x:y):ys' else [x]:ys) [[last xs]] $ reverse xs
        parseSequence xs = if length xs >= 3 then (show $ head xs) ++ "-" ++ (show $ last xs) ++ "," else foldl (\acc x -> acc ++ show x ++ ",") "" xs
    in init . concatMap parseSequence $ breakByRuns xs
-- map reverse . tail . reverse . breakByRuns $ reverse [1,2,3,5]
-- [[]] / [1,2,3,5,6]
-- [[1]] / [2,3,5,6]
-- [[1,2]] / [3,5,6]
-- [[1,2,3]] / [5,6]
-- [[1,2,3],[5]] / [6]
-- [[1,2,3],[5,6]]

rangeExtractor :: [Int] -> String
rangeExtractor = intercalate "," . map toRange . groupBy ((==) `on` uncurry (-)) . zip [1..]
  where
    toRange [(_,x)]       = show x
    toRange [(_,x),(_,y)] = show x ++ ',':show y
    toRange ((_,x):xs)    = show x ++ '-':show (snd $ last xs)

-- permutations :: String -> [String]
-- permutations "" = [""]
-- permutations xs = [x : y | x <- nub xs, y <- permutations $ delete x xs]

-- take n xs ++ [newElement] ++ drop (n+1) xs
wave :: String -> [String]
-- wave xs = zipWith (\(l, n) ys -> take n ys ++ [l] ++ drop (n + 1) ys) (filter (\(l,n) -> l /= ' ') $ zip (map toUpper xs) [0..length xs + 1]) (replicate (length xs) xs)
wave xs = let replaceN (l, n) ys= take n ys ++ [l] ++ drop (n + 1) ys
          in zipWith  replaceN (filter (\(l,n) -> l /= ' ') $ zip (map toUpper xs) [0..length xs + 1]) (replicate (length xs) xs)

uncertainty :: Char -> [String]
uncertainty x
  | x == '1' = ["1","2","4"]
  | x == '2' = ["1","2","3","5"]
  | x == '3' = ["2","3","6"]
  | x == '4' = ["1","4","5","7"]
  | x == '5' = ["2","4","5","6","8"]
  | x == '6' = ["3","5","6","9"]
  | x == '7' = ["4","7","8"]
  | x == '8' = ["5","7","8","9","0"]
  | x == '9' = ["6","8","9"]
  | x == '0' = ["8","0"]

getPINs :: String -> [String]
getPINs = mapM (concat . uncertainty)

lcs :: String -> String -> String
lcs x y = let subsX = subsequences x
              subsY = subsequences y
              compByLength x y = length x `compare` length y
          in maximumBy compByLength $ filter (`elem` subsY) subsX

-- lcs' :: String -> String -> String
-- lcs' x y = maximumBy (comparing length) (subsequences x `intersect` subsequences y)

lexiPos :: String -> Integer
lexiPos xs = let xperms = permutations xs
             in toInteger . fst . head . filter ((==xs) . snd) $ zip [1..length xperms] xperms

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

length' :: Num b => [a] -> b
length' = foldr (\x -> (+) 1) 0
-- length' [] = 0
-- length' (x:xs) = 1 + length' xs

mean :: Fractional a => [a] -> a
mean xs =  sum xs / fromIntegral (length xs)

palindrome :: [a] -> [a]
palindrome xs = xs ++ reverse xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome xs 
    | head xs /= last xs = False
    | otherwise = isPalindrome (tail $ init xs)

lengthSort :: [[a]] -> [[a]]
lengthSort [] = []
lengthSort (x:xs) = let lessThanEqual = [a | a <- xs, length a <= length x]
                        greaterThan = [a | a <- xs, length a > length x]
                    in lengthSort lessThanEqual ++ [x] ++ lengthSort greaterThan

data Direction = L | R | C deriving (Eq, Show)
type Point = (Float, Float)

direction :: Point -> Point -> Point -> Direction
direction (ax, ay) (bx, by) (cx, cy)
    | cross > 0 = L
    | cross < 0 = R
    | otherwise = C
    where vector1 = (bx - ax, by - ay)
          vector2 = (cx - bx, cy - by)
          cross = fst vector1 * snd vector2 - snd vector1 * fst vector2

directions :: [Point] -> [Direction]
directions [x, _] = []
directions zs@(x:y:z:zs') = direction x y z : directions (tail zs)

-- atan2 calculates polar angle

pointCompare :: Point -> Point -> Ordering
pointCompare (x1, y1) (x2, y2) = (y1 `compare` y2) `mappend` (x1 `compare` x2)

polarCompare :: Point -> Point -> Ordering
polarCompare p1@(x1,y1) p2@(x2, y2) = (atan2 x1 y1 `compare` atan2 x2 y2) `mappend` (magnitude p1 `compare` magnitude p2)
    where magnitude (x, y) = sqrt (x*x + y*y)

sortPolar :: Point -> [Point] -> [Point]
sortPolar (x0, y0) = sortBy (flip (\(x1, y1) (x2, y2) -> (x1-x0,y1-y0) `polarCompare` (x2-x0,y2-y0)))

foldPoints :: [Point] -> Point -> [Point]
foldPoints [x] p = [p, x]
foldPoints (x:y:ys) p
    | direction y x p == L = p:x:y:ys
    | otherwise = foldPoints (y:ys) p


grahamScan :: [Point] -> [Point]
grahamScan xs = reverse . foldl foldPoints [p0] $ (sortPolar p0 ps ++ [p0])
    where pointCompared = sortBy pointCompare xs
          p0 = head pointCompared
          ps = tail pointCompared
        --   foldPoints [x] p = [p,x]
        --   foldPoints (x:y:ys) p
        --     | direction y x p == L = p:x:y:ys
        --     | otherwise = p:y:ys
          

grahamScanDebug :: [Point] -> [[Point]]
grahamScanDebug xs = scanl foldPoints [p0] (sortPolar p0 ps ++ [p0])
    where pointCompared = sortBy pointCompare xs
          p0 = head pointCompared
          ps = tail pointCompared