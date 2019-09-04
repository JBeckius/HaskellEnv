import Data.List

data Direction = L | R | C deriving (Eq, Show)
type Point = (Double, Double)

direction :: Point -> Point -> Point -> Direction
direction (ax, ay) (bx, by) (cx, cy)
    | cross > 0 = L
    | cross < 0 = R
    | otherwise = C
    where vector1 = (bx - ax, by - ay)
          vector2 = (cx - bx, cy - by)
          cross = fst vector1 * snd vector2 - snd vector1 * fst vector2

-- directions :: [Point] -> [Direction]
-- directions [x, _] = []
-- directions zs@(x:y:z:zs') = direction x y z : directions (tail zs)

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