import Data.List
import Data.Ord (comparing)
import Data.Monoid ((<>))

simplify :: String -> String
simplify = dropWhile (== '+') . concatMap showPoly . sortBy (comparing (length . snd) <> comparing snd) . foldr (addPoly . isZero . parsePoly) [] . splitNoms
    where
    checkSplit list x = x `notElem` list
    
    splitNoms [] = []
    splitNoms xs@(x:xs')
        | x == '-' = (x : takeWhile signSplit xs') : splitNoms (dropWhile signSplit xs')
        | x == '+' = takeWhile signSplit xs' : splitNoms (dropWhile signSplit xs')
        | otherwise = takeWhile signSplit xs : splitNoms (dropWhile signSplit xs)
        where
            signSplit = checkSplit ['-','+']
    parsePoly = formatPoly . span (checkSplit ['a'..'z'])
        where formatPoly (x,y) 
                | null x || x == "+" = (1, sort y)
                | x == "-" = (-1, sort y)
                | otherwise = (read x :: Int, sort y)
    showPoly (x,y) 
        | x == 1 = '+' : y
        | x == -1 = '-' : y
        | x < 1 = show x ++ y
        | otherwise = "+" ++ show x ++ y

    comparePolys x y = (length (snd x) `compare` length (snd y)) `mappend` (snd x `compare` snd y)
    
    isZero (x,y) = if x == 0 then Nothing else Just (x,y)

    addPoly Nothing acc = acc
    addPoly (Just x@(x',y')) acc 
      | null other = x : acc
      | fst (addPolys (head other) x) == 0 = filter (not . polysEqual x) acc
      | otherwise = addPolys (head other) x : filter (not . polysEqual x) acc
        where 
            polysEqual x y = snd x == snd y 
            other = filter (polysEqual x) acc

    addPolys (x1, x2) (y1, y2) = (x1 + y1, x2)
