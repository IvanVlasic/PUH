import Data.Char
import Data.List

-- EXERCISE 1 ======================================

-- 1.1.
-- Define a function that returns a list without the first and last three
-- elements.

noTList :: [a] -> [a]
noTList xs = drop 3 . take (length xs - 3) $ xs

-- 1.2.
-- Define a function 'initals s1 s2' that takes a person's name and a surname
-- as input and returns a string consisting of person's initials.
-- initials "James" "Bond" => "J. B."

initials :: [Char] -> [Char] -> [Char]
initials s1 s2 = take 1 s1 ++ ". "  ++ take 1 s2 ++ "."

-- 1.3.
-- Define a function that concatenates two strings, so that the longest string
-- always comes first.

concatLong :: [a] -> [a] -> [a]
concatLong s1 s2
  | length s1 > length s2 = s1 ++ s2
  | otherwise             = s2 ++ s1

-- 1.4.
-- Define a function 'safeHead' that returns an empty list if 'l' is an empty
-- list, otherwise it returns its first element wrapped inside a singleton list.

safeHead :: [t] -> [t]
safeHead xs
  | null xs   = []
  | otherwise = [head xs]

-- 1.5.
-- Define a function 'hasDuplicates' that checks whether a list contains
-- duplicate elements (use 'nub').

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = xs /= nub xs

-- EXERCISE 2 ======================================

-- 2.1
-- Redefine 'doublesFromTo' so that it also works when b<a.

-- defined in lecture
doublesFromTo :: (Enum t, Num t) => t -> t -> [t]
doublesFromTo a b = [x*2 | x <- [a..b]]

doublesFromTo' :: (Enum t, Num t, Ord t) => t -> t -> [t]
doublesFromTo' a b = [x*2 | x <- [(min a b)..(max a b)]]

-- 2.2.
-- Redefine 'caesarCode s n' so that it shifts all letters a specified number of
-- positions 'n', converts all input to lowercase, and ensures that letters
-- remain within the ['a'..'z'] interval.

-- defined in lecture
caesarCode s = [succ c | c <- s, c /= ' ']

caesarCode' :: [Char] -> Int -> [Char]
caesarCode' s n = [shiftLetter n $ toLower c | c <- s, c /= ' ']

shift :: Char -> Int -> Int
shift s n = ord s + (n `mod` 26)

shiftLetter :: Int -> Char -> Char
shiftLetter n l = chr $ shift 'a' (shift l n - ord 'a')

-- EXERCISE 3 ======================================

-- 3.1.
-- Define 'letterCount' that computes the total number of letters in a string,
-- thereby ignoring the whitespaces and all words shorter than three letters.
-- You can use 'totalLength'.

-- defined in lecture
totalLength :: Foldable t => [t a] -> Int
totalLength xss = sum [length xs | xs <- xss]

letterCount :: String -> Int
letterCount = totalLength . filter ((>=3) . length) . words

-- 3.2
-- Redefine 'isPalindrome' so that it's case insensitive and works correctly
-- for strings that contain whitespaces.

-- defined in lecture
isPalindrome :: Eq a => [a] -> Bool
isPalindrome s = s == reverse s

isPalindrome' :: [Char] -> Bool
isPalindrome' s = s' == reverse s'
  where s' = map toUpper . filter (/= ' ') $ s

-- 3.3.
-- Define 'flipp xss' that takes a list of lists, reverts each individual list,
-- and concatenates all of them, but in the reverse order.
-- flip ["water","is","warm"] -> "mrawsiretaw"

flipp :: [[a]] -> [a]
flipp xss = concat [ reverse xs | xs <- reverse xss]

-- EXERCISE 4 ======================================

-- 4.1.
-- Define 'inCircle r x y' that returns the coordinates of all points within
-- the ([-10..10],[-10..10]) interval that fall inside a circle of radius
-- 'r' with center '(x,y)'.
-- Redefine the function so that it takes the resolution of the grid as an
-- additional argument.

inCircle :: (Enum t, Num t, Ord t) => t -> t -> t -> [(t, t)]
inCircle r x y = [(a,b) | a <- [-10..10], b <- [-10..10], (x - a)^2 + (y - b)^2 <= r^2]

inCircle' :: (Enum t, Num t, Ord t) => t -> t -> t -> (t, t) -> [(t, t)]
inCircle' r x y (l,p) = [(a,b) | a <- [l..p], b <- [l..p], (x - a)^2 + (y - b)^2 <= r^2]

-- 4.2.
-- Define 'steps xs' that, given a list xs=[x1,x2,..], generates the pairs
-- [(x1,x2),(x2,x3),...]. Hint: have a look at 'pairs5'.

-- given in lecture
pair5 :: [(Integer, Integer)]
pair5 = zip [1..100] [2..100]

steps :: [b] -> [(b, b)]
steps xs = zip xs (tail xs)

-- EXERCISE 5 ======================================

-- 5.1.
-- Define 'indices x xs' that returns the indices of element 'x' in list 'xs'
-- (if 'x' appears multiple times, there will be a number of such indices).
-- indices 'a' "alphabet" => [1,5]

indices :: (Enum t, Eq a, Num t) => a -> [a] -> [t]
indices x xs = [a | (a,b) <- zip [1..] xs, b == x]

-- 5.2.
-- Define 'showLineNumbers s' that prefixes all lines from string 's' with a
-- line number.
-- showLineNumbers "first line\nsecond line" => "1 first line\n2 second line\n"

showLineNumbers :: String -> String
showLineNumbers s = unlines [show a ++ " " ++ b | (a,b) <- zip [1..] $ lines s]

-- 5.3.
-- Define 'haveAlignment xs ys' that returns 'True' if 'xs' and 'ys' have
-- any identical elements that are aligned (appear at the same position in
-- both lists).
-- Define 'common xs ys' that returns the aligned subsequences.
-- haveAlignment "water" "fire" => True
-- common "witer" "fire" => "ie"

haveAlignment :: Eq a => [a] -> [a] -> Bool
haveAlignment xs = not . null . common xs

common :: Eq t => [t] -> [t] -> [t]
common xs ys = [a | (a,b) <- zip xs ys, a == b]
