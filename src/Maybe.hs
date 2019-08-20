-- Notice how there are two type declarations!
-- We're trying to establish the Maybe container as an instance of Eq
-- however, Maybe is a parameterized type.
-- We can't compare Maybes, but we can compare the contents of the Maybe container
-- Really, this is just deferring the Eq comparison from the Maybe container to the content.
-- To work, the content must be a type that supports Eq.
-- So, we add the class constraint stating that the content of Maybe must be an instance of Eq.
instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False

-- Class constraints in class declarations are used to 
-- make a type class a subclass of another type class.
-- Class constraints in instance declarations are used to
-- express requirements about the contents of some type.
