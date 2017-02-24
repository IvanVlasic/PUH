-- EXERCISE 1 ======================================

-- 1.1
-- Define 'concat3' that concatenates three strings, but drops the middle one
-- if it's shorter than 2 characters (use 'length' function).

concat3 :: [Char] -> [Char] -> [Char] -> [Char]
concat3 s1 s2 s3 = s1 ++ (if length s2 > 2 then s2 else "") ++ s3

-- 1.2
-- Give a simpler definition of 'showSalary', using only one if-then-else
-- construct.
-- Additionally check that salary is non-negative. If it's negative, return an
-- adequate message.

-- defined in lecture
showSalary :: (Eq a, Num a, Show a, Show a1) => a1 -> a -> [Char]
showSalary amount bonus
  | bonus /= 0 = "Salary is " ++ show amount ++ ", and a bonus " ++ show bonus
  | otherwise  = "Salary is " ++ show amount

showSalary' :: (Eq a1, Num a, Num a1, Ord a, Show a, Show a1) => a -> a1 -> [Char]
showSalary' amount bonus
  | amount < 0 = "Salary can't be negative."
  | otherwise  = "Salary is " ++ show amount ++ checkBonus bonus

checkBonus :: (Eq a, Num a, Show a) => a -> [Char]
checkBonus bonus = if bonus /= 0 then ", and a bonus " ++ show bonus else ""

