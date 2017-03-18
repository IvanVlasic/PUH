import Data.List
import Data.List.Split

-- =================================================

-- Problem 1.

leftFactorial :: Integer -> Integer
leftFactorial 0 = 0
leftFactorial n
  | n < 2     = n
  | otherwise = 1 + factorialSum (n - 1)

factorialSum :: Integer -> Integer
factorialSum n = sum $ scanl1 (*) [1,2..n]

-- =================================================

-- Problem 2.

factorialZeroes :: Int -> Int
factorialZeroes 0 = 0
factorialZeroes n = sum . takeWhile (> 0) . map (quot n) $ prodFive
  where prodFive = scanl1 (*) $ repeat 5

-- =================================================

-- Problem 3.

interleave :: [a] -> [a]
interleave xs = concat $ transpose [a,b]
  where n = ceiling $ fromIntegral (length xs) / 2
        (a, b) = splitAt n xs

-- =================================================

-- Problem 4.

pairs :: [a] -> [(a, a)]
pairs []     = []
pairs (x:xs) = zip (repeat x) xs ++ pairs xs

-- =================================================

-- Problem 5.

shortestSub :: Eq a => [a] -> [a]
shortestSub xs = head [x | x <- inits xs, isSub x]
  where isSub x = sum (map length (splitOn x xs)) == 0

-- =================================================

-- Problem 6.

type Timestamp = [Int]

-- a)
isValidTimestamp :: Timestamp -> Bool
isValidTimestamp x = case x of 
   [s]     -> checkSec s
   [m,s]   -> checkMin m && checkSec s
   [h,m,s] -> checkHours h && checkMin m && checkSec s
   _       -> False
  where checkSec s   = check s 60
        checkMin m   = check m 60
        checkHours h = check h 24
        check x n    = x `elem` [0..n-1]

-- b)
timestampToSec :: Timestamp -> Int
timestampToSec x
  | not $ isValidTimestamp x = error "Invalid timestamp"
  | otherwise = case x of 
    [s]     -> s
    [m,s]   -> m*60 + s
    [h,m,s] -> h*3600 + m*60 + s

-- c)
timeDiff :: Timestamp -> Timestamp -> Int
timeDiff x y 
  | (not $ isValidTimestamp x) || (not $ isValidTimestamp y) = error "Invalid timestamp"
  | otherwise = abs $ timestampToSec x - timestampToSec y

-- =================================================

-- Problem 7.

-- a)
counts :: Ord a => [a] -> [(a, Int)]
counts xs = [(x, i) | s@(x:_) <- group . sort $ xs, let i = length s]

-- b)
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' xs = (replicate n $ head xs) : group' rest
  where n    = sum [1 | x <- xs, x == head xs]
        rest = [x | x <- xs, x /= head xs]

-- c)
counts' :: Eq a => [a] -> [(a, Int)]
counts' xs = [(head x, i) | x <- group' xs, let i = length x]

-- =================================================

-- Problem 8.

type Grid = [String]

isBrokenGrid :: Foldable t => [t a] -> Bool
isBrokenGrid xs = length ( nub $ map length xs ) /= 1

lightsOutLite :: Grid -> Int
lightsOutLite grid
  | isBrokenGrid grid = error "Broken grid!"
  | otherwise         = sum [sum [1 | s <- xs, s == '1'] | xs <- grid]


-- =================================================

-- Problem 9.

-- a)
insertLetter :: String -> Char -> String -> String
insertLetter xs c ys = xs ++ [c] ++ ys

addOneLetter :: String -> [String]
addOneLetter xs = concat [[insertLetter l c r | c <- ['a'..'z']]
  | i <- [0..length xs], let (l, r) = splitAt i xs]

removeOneLetter :: String -> [String]
removeOneLetter xs = [[x | (x, i) <- zip xs [0..], i /= j] | j <- [0..(length xs - 1)]]

replaceLetter :: Int -> Char -> String -> String
replaceLetter i c xs = [choose j x | (j, x) <- zip [0..] xs]
  where choose j x
          | i == j    = c
          | otherwise = x

replaceOneLetter :: String -> [String]
replaceOneLetter xs = concat [[replaceLetter i c xs | c <- ['a'..'z']] | i <- [0..(length xs - 1)]]

swapWithNext :: Int -> String -> String
swapWithNext i xs = [choose j | j <- [0..(length xs - 1)]]
  where choose j
          | j == i     = xs !! (i + 1)
          | j == i + 1 = xs !! i
          | otherwise  = xs !! j

swapOneLetter :: String -> [String]
swapOneLetter xs = [swapWithNext i xs | i <- [0..(length xs - 2)]]

oneEdits :: String -> [String]
oneEdits xs = sort . nub $ (addOneLetter xs) ++ (removeOneLetter xs) ++ (replaceOneLetter xs) ++ (swapOneLetter xs)

-- b)

functions = [addOneLetter, removeOneLetter, replaceOneLetter, swapOneLetter]

applyFunctions f g = concat . map f . g

twoEdits :: String -> [String]
twoEdits xs = sort . nub . concat $ [concat [applyFunctions (functions !! i) (functions !! j) xs | i <- [0..3]] | j <- [0..3]]

-- Testing

compareToFile :: (String -> [String]) -> String -> FilePath -> IO Bool
compareToFile f s file = do
  list <- readFile file
  return $ f s == (read list :: [String])

testOneEdits :: IO Bool
testOneEdits = compareToFile oneEdits "hi" "oneEdits.txt"

testTwoEdits :: IO Bool
testTwoEdits = compareToFile twoEdits "hi" "twoEdits.txt"

-- =================================================

-- Problem 10.

type Seed = Int
a = 1664525
m = 2^32
c = 1013904223

-- a)
fromSeed :: Seed -> Int
fromSeed x = mod (a*x + c) m

-- b)
guess :: Seed -> Int -> IO Ordering
guess s n = do
  putStr "guess: "
  num <- readLn
  return (compare num $ fromSeed s `mod` (n + 1))
