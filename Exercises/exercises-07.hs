import Data.Char
import Data.List
import Data.Ord
import Data.List.Split

-- EXERCISE 1 ======================================

-- 1.1
-- Define 'sumEven' that adds up elements occurring at even (incl. zero)
-- positions in a list.
-- sumEven :: [Integer] -> Integer
-- sumEven [1..10] => 25

sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (even . fst) . zip [0..]

-- 1.2
-- Define 'filterWords ws s' that removes from string 's' all words contained
-- in the list 'ws'.

filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (`notElem` ws) . words

-- 1.3
-- Define 'initials3 d p s' that takes a string 's' and turns it into a string
-- of initials. The function is similar to 'initials2' but additionally delimits
-- the initials with string 'd' and discards the initials of words that don't
-- satisfy the predicate 'p'.
-- initials3 :: String -> (String -> Bool) -> String -> String
-- initials3 "." (/="that") "a company that makes everything" => "A.C.M.E."

initials3 :: String -> (String -> Bool) -> String -> String
initials3 d p = concat . map ((:d) . toUpper . head) . filter p . words

-- EXERCISE 2 ======================================

-- 2.1
-- Define 'maxDiff xs' that returns the maximum difference between consecutive
-- elements in the list 'xs'.
-- maxDiff :: [Int] -> Int
-- maxDiff [1,2,3,5,1] => 4
-- Define 'maxMinDiff' that returns the pair
-- (min_difference,max_difference).

diff :: ([Int] -> Int) -> [Int] -> Int
diff f xs = f . map (abs . uncurry (-)) . zip xs $ tail xs

maxDiff :: [Int] -> Int
maxDiff = diff maximum

minDiff :: [Int] -> Int
minDiff = diff minimum

maxMinDiff :: [Int] -> (Int, Int)
maxMinDiff xs = (minDiff xs, maxDiff xs)

-- 2.2
-- Define 'studentsPassed' that takes as input a list [(NameSurname,Score)] and
-- returns the names of all students who scored at least 50% of the maximum
-- score.

type Name = String
type Surname = String
type NameSurname = (Name, Surname)
type Score = Int

studentsPassed :: [(NameSurname, Score)] -> [Name]
studentsPassed = map (fst . fst) . filter ((>50) . snd)

-- EXERCISE 3 ======================================

-- 3.1
-- Define 'isTitleCased' that checks whether every word in a string is
-- capitalized.
-- isTitleCased :: String -> Bool
-- isTitleCased "University Of Zagreb" => True

isTitleCased :: String -> Bool
isTitleCased = all isUpper . map head . words

-- 3.2
-- Define 'sortPairs' that sorts the list of pairs in ascending order with
-- respect to the second element of a pair.

sortPairs :: Ord b => [(a,b)] -> [(a,b)]
sortPairs = sortBy (comparing snd)

-- 3.3
-- Define 'filename' that extracts the the name of the file from a file path.
-- filename :: String -> String
-- filename "/etc/init/cron.conf" => "cron.conf"

fileName :: String -> String
fileName = last . splitOn "/"

-- 3.4
-- Define 'maxElemIndices' that returns the indices of the maximum element in a
-- list. Return "empty list" error if the list is empty.
-- maxElemIndices :: Ord a => [a] -> [Int]
-- maxElemIndices [1,3,4,1,3,4] => [2,5]

maxElemIndices :: Ord a => [a] -> [Int]
maxElemIndices xs = findIndices (== m) xs
  where m = maximum xs

-- EXERCISE 4 ======================================

-- 4.1
-- Define 'elem' using 'foldr'.

elem' :: (Eq a, Foldable t) => a -> t a -> Bool
elem' e = foldr (\x acc -> x == e || acc) False

-- 4.2
-- Define 'reverse' using 'foldr'.

reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []

-- 4.3
-- Using 'foldr' define 'nubRuns' that removes consecutively repeated elements
-- from a list.
-- nubRuns :: Eq a => [a] -> [a]
-- nubRuns "Mississippi" => "Misisipi"

nubRuns :: Eq a => [a] -> [a]
nubRuns = foldr f []
  where
    f x [] = [x]
    f x ys@(y:_)
      | x == y = ys
      | otherwise = x:ys

-- EXERCISE 5 ======================================


-- 5.1
-- Define 'reverse' using 'foldl'.

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

-- 5.2
-- Using 'foldl' define function 'sumEven' from problem 1.1.

sumEven' :: [Integer] -> Integer
sumEven' = foldl (\acc (i,x) -> if even i then x + acc else acc) 0 . zip [0..]

-- 5.3
-- Using 'foldl' define maxUnzip :: [(Int,Int)] -> (Int,Int)
-- that returns the maximum element at first position in a pair and maximum
-- element at second position in the pair. In other words, the function
-- should
-- be equivalent to:
-- maxUnzip zs = (maximum xs, maximum ys)
-- where (xs,ys) = unzip zs
-- Return "empty list" error if the list is empty.

maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip [] = error "List is empty"
maxUnzip ((x,y):xs) = foldl (\(a,b) (x,y) -> (max a x, max b y)) (x,y) xs
