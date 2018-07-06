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
-- NOTES
-- * myLast _:xs = ... this gives and error
-- * myLast (_:xs) = ... enclose pattern matching by ()

-- 1.0: explicit recursion
myLast :: [a] ->  a
myLast [] = error "Empy List; there is no last element to return"
myLast [x] = x -- | base case;
myLast (_:xs) = myLast xs -- | recur until we come to [x] the base case

-- 1.1: composing functions
myLast' = head . reverse

-- 1.2: using index
myLast'' [] = error "Empy List; there is no last element to return"
myLast'' xs = xs !! (length xs -1)


-- 1.3: using foldr1
-- * const: takes two arguments and returns the first arg, always.
-- * flip: takes bi-function and two args (for the bi-fn) then it applies bi-fn
--         on the two args but after swaping them.
-- * (flip const) returns the second argument.
myLast''' xs = foldr1 (flip const) xs

------
------
------

-- | Problem 2
-- | (*) Find the last but one element of a list.
-- |
-- | (Note that the Lisp transcription of this problem is incorrect.)
-- |
-- | Example in Haskell:
-- |
-- | Prelude> myButLast [1,2,3,4]
-- | 3
-- | Prelude> myButLast ['a'..'z']
-- | 'y'
myButLast :: [a] -> a
myButLast [] = error "Empty List"
myButLast [x] = error "List contains one element; there is not but last"
myButLast xs = xs !! (length xs -2)

myButLast' = myLast . init

myButLast'' = head . tail . reverse

-- * If the tail is 1 element; return the head
myButLast''' [] =  error "Empty List"
myButLast''' [x] = error "List contains one element; there is not but last"
myButLast''' (x:xs)
  | length xs == 1 = x
  | otherwise = myButLast''' xs

myButLast'''' [] =  error "Empty List"
myButLast'''' [x] = error "List contains one element; there is not but last"
myButLast'''' list
  | length (tail list) == 1 = head list
  | otherwise = myButLast'''' $ tail list



------
------
------


-- | Problem 3
-- | (*) Find the K'th element of a list. The first element in the list is number 1.
-- |
-- | Example:
-- |
-- | * (element-at '(a b c d e) 3)
-- | c
-- | Example in Haskell:
-- |
-- | Prelude> elementAt [1,2,3] 2
-- | 2
-- | Prelude> elementAt "haskell" 5
-- | 'e'
elementAt :: [a] -> Int -> a
elementAt (x:_)  1 =  x  -- * if 0 wanted; return the first element
elementAt (_:xs) n =  elementAt xs  (n-1)


elementAt' [] _ = error "Empty List"
elementAt' (x:_) 1 = x
elementAt' (_:xs) n
  | n < 1 = error "index cannot be < 1"
  | otherwise = elementAt' xs (n-1)

elementAt'' [] _ = error "Empty List"
elementAt'' xs n= last $ take n xs
