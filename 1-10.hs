-- | Problem 1
-- | (*) Find the last element of a list.
-- |
-- | (Note that the Lisp transcription of this problem is incorrect.)
-- |
-- | Example in Haskell:
-- |
-- | Prelude> myLast [1,2,3,4]
-- | 4
-- | Prelude> myLast ['x','y','z']
-- | 'z'
myLast :: [a] ->  a
myLast [] = error "Empy List; there is no last element to return"
myLast [x] = x -- | base case;
myLast (_:xs) = myLast xs -- | recur until we come to [x] the base case
