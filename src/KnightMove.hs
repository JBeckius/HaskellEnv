module KnightMove
    (
        KnightPos(..),
        moveKnight,
        in3,
        getThereIn3
    ) where

type KnightPos = (Int, Int)
-- type KnightMoves = [(Int, Int)]
type Grid = (Int, Int)
type History = String

grid :: Grid
grid = (8, 8)

-- knightMoves :: KnightMoves
-- knightMoves = nub $ [1,-1] >>= \x -> [2, -2] >>= \y -> [(x, y)] ++ ([2, -2] >>= \x -> [1, -1] >>= \y -> [(x, y)])
-- --[ (2, 1), (1, 2), (2, -1), (-1, -2), (1, -2), (2, -1)]

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = filter onBoard
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
    ]
    where onBoard (c, r) = c `elem` [1..fst grid] && r `elem` [1..snd grid]

moveKnightHistory :: History -> KnightPos -> [(History, KnightPos)]
moveKnightHistory history (c, r) = filter onBoard
    [(history ++ "(2, -1)",(c+2,r-1)),(history ++ "(2, 1)",(c+2,r+1))
    ,(history ++ "(-2, -1)",(c-2,r-1)),(history ++ "(-2, 1)",(c-2,r+1))
    ,(history ++ "(1, -2)",(c+1,r-2)),(history ++ "(1, 2)",(c+1,r+2))
    ,(history ++ "(-1, -2)",(c-1,r-2)),(history ++ "(-1, +2)",(c-1,r+2))
    ]
    where onBoard (h, (c, r)) = c `elem` [1..fst grid] && r `elem` [1..snd grid]

in3 :: KnightPos -> [(History, KnightPos)]
in3 start = do
    (historyFst, first) <- moveKnightHistory "" start
    (historySnd, second) <- moveKnightHistory historyFst first
    moveKnightHistory historySnd second


-- inX :: KnightPos -> [KnightPos]
-- inX start = 

getThereIn3 :: KnightPos -> KnightPos -> [(History, KnightPos)]
getThereIn3 startPos endPos = filter (\x -> snd x == endPos) $ in3 startPos