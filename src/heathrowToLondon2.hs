import Data.List

main = do
    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        path = optimalPath roadSystem
        pathString = concatMap (show . fst) path
        pathTIme = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "Time taken: " ++ show pathTIme

data Section = Section { getA :: Int, getB :: Int, getC :: Int }
        deriving (Show)

type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30
                   , Section 5  90 20
                   , Section 40 2  25
                   , Section 10 8  0
                   ]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem = 
    let (bestPathA, bestPathB) = foldl roadStep ([], []) roadSystem
    in if sum (map snd bestPathA) <= sum (map snd bestPathB)
        then reverse bestPathA
        else reverse bestPathB

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let pathATime = sum (map snd pathA)
        pathBTime = sum (map snd pathB)
        straightToA = pathATime + a
        crossToA = pathBTime + b + c
        straightToB = pathBTime + b
        crossToB = pathATime + a + c
        newPathToA = if straightToA <= crossToA
                        then (A, a):pathA
                        else (C, c):(B, b):pathB
        newPathToB = if straightToB <= crossToB
                        then (B, b):pathB
                        else (C, c):(A, a):pathA
    in (newPathToA, newPathToB)

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = []
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)