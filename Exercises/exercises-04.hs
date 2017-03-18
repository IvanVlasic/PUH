import Data.Char
import Data.List

-- EXERCISE 1 ======================================

-- 1.1
-- Define 'headHunter xss' that takes the head of the first list element. If
-- the first element has no head, it takes the head of the second element.
-- If the second element has no head, it takes the head of the third
-- element.
-- If none of this works, the function returns an error.

headHunter :: [[a]] -> a
headHunter ((x:_):_)      = x
headHunter (_:(x:_):_)    = x
headHunter (_:_:(x:xs):_) = x
headHunter _              = error "First three elements don't have a head."

-- 1.2
-- Define 'firstColumn m' that returns the first column of a matrix.
-- firstColumn [[1,2],[3,4]] => [1,3]
-- Check what happens if the input is not a valid matrix.

firstColumn :: [[t]] -> [t]
firstColumn m = [x | (x:_) <- m]

-- 1.3
-- Define 'shoutOutLoud' that repeats three times the initial letter of each
-- word in a string.
-- shoutOutLoud :: String -> String
-- shoutOutLoud "Is anybody here?" => "IIIs aaanybody hhhere?"

shoutOutLoud :: String -> String
shoutOutLoud s = unwords [x:x:x:xs | x:xs <- words s]

-- EXERCISE 2 ======================================

-- 2.1
-- Define 'pad' that pads the shorter of two the strings with trailing spaces
-- and returns both strings capitalized.
-- pad :: String -> String -> (String,String)
-- pad "elephant" "cat" => ("Elephant","Cat     ")

pad :: String -> String -> (String, String)
pad a b
  | null a && null b = error "Both strings are empty."
  | otherwise = (cap . fill (lb - la) $ a, cap . fill (la - lb) $ b)
  where
    la = length a
    lb = length b
    fill n xs = xs ++ replicate n  ' '
    cap (x:xs) = toUpper x : xs

-- 2.2
-- Define 'quartiles xs' that returns the quartiles (q1,q2,q3) of a given list.
-- The quartiles are elements at the first, second, and third quarter of a
-- list sorted in ascending order. (You can use the built-int 'splitAt' function
-- and the previously defined 'median' function.)
-- quartiles :: [Int] -> (Double,Double,Double)
-- quartiles [3,1,2,4,5,6,8,0,7] => (1.5, 4.0, 6.5)

-- defined in lecture
median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty list"
median xs
 | odd l     = realToFrac $ ys !! h
 | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
 where
   l  = length xs
   h  = l `div` 2
   ys = sort xs

quartiles :: [Int] -> (Double, Double, Double)
quartiles [] = error "Empty list"
quartiles xs
  | l < 3     = error "List contains less than 3 elements"
  | otherwise = (median left, median xs, median right)
  where
    l = length xs
    (a, b) = (quot l 2, if even l then 2 else 1)
    ys = sort xs
    part x y = [ys !! i | i <- [x..y]]
    (left, right) = (part 0 (a - b), part (a + 1) (l - 1))

-- EXERCISE 3 ======================================

-- Redo exercise 2 using 'let' instead of 'where'.

-- 3.1

pad' :: String -> String -> (String, String)
pad' a b =
  let la = length a
      lb = length b
      fill n xs = xs ++ replicate n ' '
      cap (x:xs) = toUpper x : xs
  in case () of
   _ | null a && null b -> error "Both strings are empty."
     | otherwise -> (cap . fill (lb - la) $ a, cap . fill (la - lb) $ b)

-- 3.2

quartiles' :: [Int] -> (Double, Double, Double)
quartiles' [] = error "Empty list"
quartiles' xs =
  let l = length xs
      (a, b) = (quot l 2, if even l then 2 else 1)
      ys = sort xs
      part x y = [ys !! i | i <- [x..y]]
      (left, right) = (part 0 (a - b), part (a + 1) (l - 1))
  in case () of
  _ | l < 3 -> error "List contains less than 3 elements"
    | otherwise -> (median left, median xs, median right)

-- EXERCISE 4 ======================================

-- 4.1
-- Write a function that takes in a pair (a,b) and a list [c] and returns the
-- following string:
-- "The pair [contains two ones|contains one one|does not contain a single one]
-- and the second element of the list is <x>"

pac :: (Eq a, Num a, Show a1) => (a, a) -> [a1] -> String
pac p (_:y:_) =
  "The pair " ++
  case p of
    (1, 1) -> "contains two ones"
    (1, _) -> "contains one one"
    (_, 1) -> "contains one one"
    _      -> "does not contain a single one"
  ++ " and the second element of the list is " ++ show y
pac _ _ = error "List doesn't have second element"
