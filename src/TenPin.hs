import Data.Char
import Data.List

data FrameType = Strike | Spare | Basic

bowlingScore :: String -> Int
bowlingScore = calc . concatMap parse . words
    where parse [] = []
          parse xs@[x,y,z]
            | x == 'X' = parse [x] ++ parse [y,z]
            | y == '/' = parse [x,y] ++ parse [z]
          parse x
            | head x == 'X' = 10 : parse (tail x)
            | '/' `elem` x = [digitToInt (head x), 10 - digitToInt (head x)]
            | otherwise = map digitToInt x
          calc [] = 0
          calc [x] = x
          calc [x,y] = x + y
          calc [x,y,z] = x + y + z
          calc (x:y:z:zs) 
            | x == 10 = x + y + z + calc (y:z:zs)
            | x + y == 10 = x + y + z + calc (z:zs)
            | otherwise = x + y + calc (z:zs)


bowlingScore2 :: String -> [Int]
bowlingScore2 myFrames = map (score . concat) $ init $ tails $ words myFrames
    where
    score ('X':_:'/':_) = 20
    score ('X':i:j:_) = 10 + score' i + score' j
    score (_:'/':j:_) = 10 + score' j
    score (i:j:_) = digitToInt i + digitToInt j
    score s = error $ "impossible: " ++ s

    score' 'X' = 10
    score' d = digitToInt d


-- calc :: [Int] -> Int
-- calc [x] = x
-- calc [x,y] = x + y
-- calc [x,y,z] = x + y + z
-- calc (x:y:z:zs) 
--     | x == 10 = x + y + z + calc (y:z:zs)
--     | x + y == 10 = x + y + z + calc (z:zs)
--     | otherwise = x + y + calc (z:zs)