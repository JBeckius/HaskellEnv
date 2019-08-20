-- Functor = Anything that can be mapped over
class Functor f where
    fmap :: (a -> b) -> f a -> f b

-- Functor wants a type constructor that takes one type, not a concrete type.
-- This is why these definitions aren't written as 
-- instance Functor [a] where
-- or
-- instance Functor (Maybe a) where

-- List is an instance of the Functor type class!
-- map :: (a -> b) -> [a] -> [b]
instance Functor [] where
    fmap = map

-- Types that can act like a box can be functors
-- A list is just a box that contains values, including other lists
-- Maybe is a box that contains Nothing or a value, including other Maybes.

instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
