import Data.Char

-- =================================================

-- Problem 1.

isNegative :: (Num a, Ord a) => a -> Bool
isNegative n = n < 0

isInRange :: Ord a => a -> a -> a -> Bool
isInRange n a b = n >= a && n <= b

strlenInRange :: Foldable t => t a -> Int -> Int -> Bool
strlenInRange s a b
  | isNegative a || isNegative b = error "String length cannot be a negative number."
  | otherwise = isInRange (length s) a b

-- =================================================

-- Problem 2.

isHereAGreater :: Ord a => [a] -> Int -> a -> Bool
isHereAGreater xs index value
  | index  >= l || index < 0 = False
  | otherwise = xs !! index > value
  where l = length xs

-- =================================================

-- Problem 3.

wordFilter :: IO ()
wordFilter = do
  sentence <- getLine
  word <- getLine
  let filtered = unwords [ w | w <- words sentence, w /= word]
  putStrLn filtered

-- =================================================

-- Problem 4.

ord3 :: Ord a => a -> a -> a -> [a]
ord3 a b c = if a >= b
               then if a >= c
                 then if b >= c
                   then [c,b,a]
                   else [b,c,a]
                 else [b,a,c]
             else if b >= c
               then if a >= c
                 then [c,a,b]
                 else [a,c,b]
               else [a,b,c]

-- =================================================

-- Problem 5.

-- a)
norm :: Floating r => (r, r) -> r
norm (a, b) = sqrt $ a^2 + b^2

-- b)
add :: (Num t, Num t1) => (t, t1) -> (t, t1) -> (t, t1)
add (a1, b1) (a2, b2) = (a1+a2, b1+b2)

-- c)
scalarMult :: Num t => (t, t) -> t -> (t, t)
scalarMult (a, b) s = (a*s, b*s)

-- d)
dot :: Num a => (a, a) -> (a, a) -> a
dot (a1, b1) (a2, b2) = a1*a2 + b1*b2

-- =================================================

-- Problem 6.

asciiRange :: Char -> Char -> [(Char, Int)]
asciiRange a b = zip [a..b] [(ord a)..(ord b)]

-- =================================================

-- Problem 7.

incn :: Int -> [Char] -> [Char]
incn n xs = [chr $ ord x + n | x <- xs]
