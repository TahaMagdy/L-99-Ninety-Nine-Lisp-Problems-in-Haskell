-- * Take a look at Data.List;
-- * t has got tons of useful functions you can learn from.
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
myLast [x] = x --  base case;
myLast (_:xs) = myLast xs --  recur until we come to [x] the base case

-- 1.1: composing functions
myLast' :: [c] -> c
myLast' = head . reverse

-- 1.2: using index
myLast'' :: [c] -> c
myLast'' [] = error "Empy List; there is no last element to return"
myLast'' xs = xs !! (length xs -1)


-- 1.3: using foldr1
-- * const: takes two arguments and returns the first arg, always.
-- * flip: takes bi-function and two args (for the bi-fn) then it applies bi-fn
--         on the two args but after swaping them.
-- * (flip const) returns the second argument.
myLast''' :: [c] -> c
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
myButLast [_] = error "List contains one element; there is not but last"
myButLast xs = xs !! (length xs -2)

myButLast' :: [c] -> c
myButLast' = myLast . init

myButLast'' :: [c] -> c
myButLast'' = head . tail . reverse

-- If the tail is 1 element; return the head
myButLast''' :: [p] -> p
myButLast''' [] =  error "Empty List"
myButLast''' [_] = error "List contains one element; there is not but last"
myButLast''' (x:xs)
  | length xs == 1 = x
  | otherwise = myButLast''' xs


myButLast'''' :: [a] -> a
myButLast'''' [] =  error "Empty List"
myButLast'''' [_] = error "List contains one element; there is not but last"
myButLast'''' list
  | length (tail list) == 1 = head list
  | otherwise = myButLast'''' $ tail list



------
------
------


-- Problem 3
-- (*) Find the K'th element of a list. The first element in the list is number 1.
-- 
-- Example:
-- 
-- * (element-at '(a b c d e) 3)
-- c
-- Example in Haskell:
-- 
-- Prelude> elementAt [1,2,3] 2
-- 2
-- Prelude> elementAt "haskell" 5
-- 'e'
elementAt :: [a] -> Int -> a
elementAt [] _     = error "Empty List"
elementAt (x:_)  1 =  x  -- if 0 wanted; return the first element
elementAt (_:xs) n =  elementAt xs  (n-1)


elementAt' :: [a] -> Int -> a
elementAt' [] _ = error "Empty List"
elementAt' (x:_) 1 = x
elementAt' (_:xs) n
  | n < 1 = error "index cannot be < 1"
  | otherwise = elementAt' xs (n-1)

elementAt'' :: [a] -> Int -> a
elementAt'' [] _ = error "Empty List"
elementAt'' xs n = last $ take n xs

------
------
------

-- |   Problem 4
-- |   (*) Find the number of elements of a list.
-- |
-- |   Example in Haskell:
-- |
-- |   Prelude> myLength [123, 456, 789]
-- |   3
-- |   Prelude> myLength "Hello, world!"
-- |   13
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs


myLength' :: [a] -> Int
myLength' xs = sum $ replicate (length xs) 1

myLength'' :: [a] -> Int
myLength'' =  sum . map (\_ -> 1)

------
------
------

-- |   Problem 5
-- |   (*) Reverse a list.
-- |
-- |   Example in Haskell:
-- |
-- |   Prelude> myReverse "A man, a plan, a canal, panama!"
-- |   "!amanap ,lanac a ,nalp a ,nam A"
-- |   Prelude> myReverse [1,2,3,4]
-- |   [4,3,2,1]
myReverse :: [a] -> [a]
myReverse []  = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse (xs) ++ [x]


------
------
------

-- | Problem 6
-- | (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
-- |
-- | Example in Haskell:
-- |
-- | *Main> isPalindrome [1,2,3]
-- | False
-- | *Main> isPalindrome "madamimadam"
-- | True
-- | *Main> isPalindrome [1,2,4,8,16,8,4,2,1]
-- | True
isPalindrome :: (Ord a) => [a] -> Bool
isPalindrome list =
  list == reverse list


isPalindrome' :: (Ord a) => [a] -> Bool
isPalindrome' xs =
  firstHalf xs == secondHalf xs
  where
    len  = length xs
    n = div len 2
    firstHalf  l = take n l
    secondHalf l = drop (n + mod len 2) l

------
------
------

-- | Problem 7
-- | (**) Flatten a nested list structure.
-- |
-- | Transform a list, possibly holding lists as elements
-- | into a `flat' list by replacing each list with its elements (recursively).
-- |
-- | Example:
-- |
-- | * (my-flatten '(a (b (c d) e)))
-- | (A B C D E)
-- | Example in Haskell:
-- |
-- | We have to define a new data type, because lists in Haskell are homogeneous.
-- |
-- | data NestedList a = Elem a | List [NestedList a]
-- | *Main> flatten (Elem 5)
-- | [5]
-- | *Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- | [1,2,3,4,5]
-- | *Main> flatten (List [])
-- | []
data NestedList a = Elem a
                  | List [NestedList a] deriving Show

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List (xs))

------
------
------

-- |  Problem 8
-- |  (**) Eliminate consecutive duplicates of list elements.
-- |
-- |  If a list contains repeated elements they should be replaced with
-- |  a single copy of the element. The order of the elements should not be changed.
-- |
-- |  Example:
-- |
-- |  * (compress '(a a a a b c c a a d e e e e))
-- |  (A B C A D E)
-- |  Example in Haskell:
-- |
-- |  > compress "aaaabccaadeeee"
-- |  "abcade"
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
  | x == head xs      = compress xs  -- Ignoring the duplicated x
  | otherwise         = [x] ++ compress xs -- Building up the un-duplicated x

------
------
------
-- | Problem 9
-- | (**) Pack consecutive duplicates of list elements into sublists.
-- | If a list contains repeated elements they should be placed in separate sublists.
-- |
-- | Example:
-- |
-- | * (pack '(a a a a b c c a a d e e e e))
-- | ((A A A A) (B) (C C) (A A) (D) (E E E E))
-- | Example in Haskell:
-- |
-- | *Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a',
-- |             'a', 'd', 'e', 'e', 'e', 'e']
-- | ["aaaa","b","cc","aa","d","eeee"]
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x:takeWhile (==x) xs) : pack (dropWhile (==x) xs)

------
------
------


-- Problem 10
-- (*) Run-length encoding of a list. Use the result of problem P09 to
-- implement the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E)
-- where N is the number of duplicates of the element E.
-- 
-- Example:
-- 
-- * (encode '(a a a a b c c a a d e e e e))
-- ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
-- Example in Haskell:
-- 
-- encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode :: Eq b => [b] -> [(Int, b)]
encode list = f $ pack list
  where
    f l = map (\xs -> (length xs, head xs)) l
