-- https://github.com/TahaMagdy/L-99-Ninety-Nine-Lisp-Problems-in-Haskell/blob/master/1-10.hs
-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
-- * Take a look at Data.List;

-- | Problem 1: Find the last element of a list.
-- Note: myLast _:xs = ... this gives and error
-- Note: myLast (_:xs) = ... enclose pattern matching by ()
-- Explicit Recursion
myLast1 :: [a] ->  a
myLast1 [] = error "Empy List; there is no last element to return"
myLast1 [x] = x --  base case;
myLast1 (_:xs) = myLast1 xs --  recur until we come to [x] the base case

-- The last element is the (head) of a (reverse)
myLast2 :: [c] -> c
myLast2 = head . reverse

-- Using index
myLast3 :: [c] -> c
myLast3 [] = error "Empy List; there is no last element to return"
myLast3 xs = xs !! (length xs -1)

-- Using foldr1
-- ComeBackHere
-- * const: takes two arguments and returns the first arg, always.
-- * flip: takes bi-function and two args (for the bi-fn) then it applies bi-fn
--         on the two args but after swaping them.
-- * (flip const) returns the second argument.
myLast4 :: [c] -> c
myLast4 xs = foldr1 (flip const) xs


-- | Problem 2: Find the last but one element of a list.
myButLast1 :: [a] -> a
myButLast1 [] = error "Empty List"
myButLast1 [_] = error "List contains one element; there is not but last"
myButLast1 xs = xs !! (length xs -2)

-- myButLast is the last of the init
myButLast2 :: [c] -> c
myButLast2 = myLast1 . init

myButLast3 :: [c] -> c
myButLast3 = head . tail . reverse

-- myButLast is the head of a list in the form (x:y:[])
myButLast4 :: [p] -> p
myButLast4 [] =  error "Empty List"
myButLast4 [_] = error "List contains one element; there is not but last"
myButLast4 (x:xs)
  | length xs == 1 = x
  | otherwise = myButLast4 xs

myButLast5 :: [a] -> a
myButLast5 [] = error "[]"
myButLast5 [_] = error "[_]"
myButLast5 (j:_:[]) = j
myButLast5 (_:xs) = myButLast5 xs

myButLast6 :: [a] -> a
myButLast6 [] =  error "Empty List"
myButLast6 [_] = error "List contains one element; there is not but last"
myButLast6 list
  | length (tail list) == 1 = head list
  | otherwise = myButLast5 $ tail list


-- | Problem 3: Find the K'th element of a list.
elementAt1 :: [a] -> Int -> a
elementAt1 [] _     = error "Empty List"
elementAt1 (x:_)  1 =  x
elementAt1 (_:xs) n 
  | n <= 0 = error "Index cannot be a negative number"
  | otherwise = elementAt1 xs (n-1) -- remove (n-1) heads

elementAt2 :: [a] -> Int -> a
elementAt2 [] _ = error "Empty List"
elementAt2 xs n = last $ take n xs

-- | Problem 4: Find the number of elements of a list.
myLength1 :: [a] -> Int
myLength1 [] = 0
myLength1 (_:xs) = 1 + myLength1 xs

myLength2 :: [a] -> Int
myLength2 xs = sum $ replicate (length xs) 1

myLength3 :: [a] -> Int
myLength3 =  sum . map (\_ -> 1)


-- | Problem 5: Reverse a list.
-- ComeBackHere you may use (:) instead of (++)
myReverse :: [a] -> [a]
myReverse []  = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse (xs) ++ [x]


-- | Problem 6: Find out whether a list is a palindrome. 
-- "121" is plindrome
-- A plindrome list is the remains the same if you reversed it
isPalindrome1 :: (Ord a) => [a] -> Bool
isPalindrome1 list = list == reverse list


-- | Problem 7: Flatten a nested list structure.
-- ComeBackHere


-- |  Problem 8: Eliminate consecutive duplicates of list elements.
compress1 :: (Eq a) => [a] -> [a]
compress1 [] = []
compress1 [x] = [x]
compress1 (x:xs)
  | x == head xs = compress1 xs  -- Ignoring the duplicated x
  | otherwise    = [x] ++ compress1 xs -- Building up the un-duplicated x


-- | Problem 9: Pack consecutive duplicates of list elements into sublists.
pack1 :: Eq a => [a] -> [[a]]
pack1 [] = []
pack1 (x:xs) = (x:takeWhile (==x) xs) : pack1 (dropWhile (==x) xs)


-- | Problem 10: Run-length encoding of a list. 
-- λ> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode :: Eq b => [b] -> [(Int, b)]
encode list = f $ pack1 list
  where
    f l = map (\xs -> (length xs, head xs)) l