import Data.Char
import Data.List
import Data.List.Split

-- =================================================

-- Problem 1.

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat [[x,y] | (x, y) <- zip xs ys]

-- =================================================

-- Problem 2.

slice :: Int -> Int -> [a] -> [a]
slice m n xs
  | a < 0 || b >= l  = error "Slice index out of range"
  | otherwise = [xs !! i | i <- [a..b]] 
  where l = length xs
        (a, b) = (min m n, max m n)

-- =================================================

-- Problem 3.

decamel :: String -> String
decamel s
  | null s = error "identifier is empty"
  | (length $ words s) > 1  = error "input not in camel case format"
  | otherwise = dropWhile isSpace $ concat [format c | c <- s] 

format :: Char -> [Char]
format c
  | isUpper c = ' ' : [toLower c]
  | otherwise = [c]

-- =================================================

-- Problem 4.

-- a)
count :: Eq a => [a] -> a -> Int
count s a = sum [1 | c <- s, c == a]

-- b)
removeUniques :: Eq a => [a] -> [a]
removeUniques xs = [c | c <- xs, count xs c /= 1]

-- =================================================

-- Problem 5.

type Mask = String

mask :: String -> Mask -> String
mask xs m
  | null m = ""
  | otherwise = [x | (x, i) <- zip xs (cycle m), i == '1']

-- =================================================

-- Problem 6.

type Point = (Int, Int)
type Friend = (Point, String)

findFriend :: Point -> [Friend] -> String
findFriend _ [] = error "Nobody exists to be your friend"
findFriend (a,b) (x:xs) = findFriend' (a,b) x xs

findFriend' _ (_,n) [] = n
findFriend' (a,b) ((a1,b1),n) (x:xs)
  | dist1 < dist2 = findFriend' (a,b) ((a1,b1),n) xs
  | otherwise     = findFriend' (a,b) ((a2,b2),m) xs
  where ((a2,b2),m) = x
        dist1 = distance (a, b) (a1, b1)
        dist2 = distance (a, b) (a2, b2)

distance :: Num a => (a, a) -> (a, a) -> a
distance (x1,y1) (x2,y2) = (x1 - x2)^2 + (y1 - y2)^2

-- =================================================

-- Problem 7.

-- a)
mulTable :: Int -> [[Int]]
mulTable n
  | n <= 0    = error "Given number is lesser than 1"
  | otherwise = mulTable' 1 n

mulTable' :: Int -> Int -> [[Int]]
mulTable' n m
  | n == m + 1 = []
  | otherwise  = take m [n,n*2..] : mulTable' (n + 1) m

-- b)
leftpad :: Show a => Int -> a -> String
leftpad n a
  | n < 0     = error "Cannot pad to negative length"
  | n < l     = error $ s ++ " does not fit into " ++ show n ++ " characters"
  | otherwise = (take (n - l) $ repeat ' ') ++ s
  where s = show a
        l = length s

-- c)
prettyTable :: Show a => [[a]] -> IO ()
prettyTable xs = putStrLn $ intercalate "\n" [concat [leftpad (l + 1) y | y <- x] | x <- xs]  
  where l = maximum . concat $ [[length . show $ y | y <- x] | x <- xs]

-- =================================================

-- Problem 8.

-- CSVUtils.hs

-- =================================================

-- Problem 9.

-- a)
wc :: FilePath -> IO ()
wc path = do
  text <- readFile path
  let nLines = show . length . lines $ text
  let nWords = show . length . words $ text
  let nChars = show . length $ text
  putStrLn $ intercalate " " [nLines, nWords, nChars]

-- b)
paste :: FilePath -> FilePath -> IO ()
paste p1 p2 = do
  f1 <- readFile p1
  f2 <- readFile p2
  let l1 = lines f1
  let l2 = lines f2
  putStr $ unlines [a ++ "\t" ++ b | (a, b) <- zip l1 l2]

-- c)
cut :: String -> Int -> FilePath -> IO ()
cut delim n path = do
  f <- readFile path
  let parts = [splitOn delim x | x <- lines f]
  putStr $ unlines . map (!! (n - 1)) $ parts
