module List
    (
        List(..)
    ) where
infixr 5 :-:
infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x:-:xs) ^++ ys = x :-: (xs ^++ ys) 
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
-- or     = Empty | Cons a { listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)



