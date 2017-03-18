import Data.List

data Shape =
    Circle Double Double Double
  | Rectangle Double Double Double Double
  deriving Show

data Point = Point Double Double deriving Show

data Shape2 = Circle2 Point Double | Rectangle2 Point Point deriving Show

-- EXERCISE 1 ======================================

-- 1.1
-- Define a 'Date' structure with the appropriate fields.
-- Define a function that shows a date in the DD.MM.YYYY format (without
-- leading zeroes).
-- showDate :: Date -> String

data Date = Date Int Int Int

showDate :: Date -> String
showDate (Date d m y) = show d ++ "." ++ show m ++ "." ++ show y

-- 1.2
-- Define a function
-- translate :: Point -> Shape2 -> Shape2
-- that translates a shape into the direction of vector (x,y).

translate :: Point -> Shape2 -> Shape2
translate (Point x y) (Circle2 (Point x1 y1) r) = Circle2 (Point (x1 + x) (y1 + y)) r
translate (Point x y) (Rectangle2 (Point x1 y1) (Point x2 y2)) =
  Rectangle2 (Point (x1 + x) (y1 + y)) (Point (x2 + x) (y2 + y))

-- 1.3
-- Write a function 'inShape' that tests whether a point is contained within a
-- given shape (or is on its border).
-- inShape :: Shape2 -> Point -> Bool
-- Write a function 'inShapes' that tests if the point is within any shape
-- from the list of shapes.
-- inShapes :: [Shape2] -> Point -> Bool

inShape :: Shape2 -> Point -> Bool
inShape (Circle2 (Point x1 y1) r) (Point x y) = sqrt ((x1 - x)**2 + (y1 - y)**2) <= r**2
inShape (Rectangle2 (Point x1 y1) (Point x2 y2)) (Point x y) =
  (x1 >= x && x2 <= x) && (y1 >= y && y2 <= y)

-- 1.4
-- Define your type 'Vehicle' that can be a 'Car', 'Truck',
-- 'Motorcycle', or 'Bicycle'. The first three store a name of the
-- manufacturer (String) and horsepower (Double).
-- Write a function 'totalHorsepower' that adds up the horsepower of the
-- vehicles, assuming that bicycle's horsepower is 0.2.

data Vehicle =
    Car String Double
  | Truck String Double
  | Motorcycle String Double
  | Bicycle

horsePower :: Vehicle -> Double
horsePower (Car _ hp) = hp
horsePower (Truck _ hp) = hp
horsePower (Motorcycle _ hp) = hp
horsePower Bicycle = 0.2

totalHorsePower :: [Vehicle] -> Double
totalHorsePower = sum . map horsePower

-- EXERCISE 2 ======================================

data Level = Bachelor | Master | PhD deriving (Show, Eq)

data Student = Student
  { firstName :: String
  , lastName  :: String
  , studentId :: String
  , level     :: Level
  , avgGrade  :: Double } deriving Show

-- 2.1
-- Define a function that increases the average grade of the student by 1.0,
-- but not above 5.0.
-- improveStudent :: Student -> Student

improveStudent :: Student -> Student
improveStudent s@(Student {avgGrade=avg})
  | avg <= 4 = s{avgGrade=avg+1}
  | otherwise = s

-- 2.2
-- Write a function to compute the average grade of students for the different
-- study levels.
-- avgGradePerLevels :: [Student] -> (Double,Double,Double)

getAvg :: Level -> [Student] -> Double
getAvg l ss = s / fromIntegral (length fs)
  where
    fs = filterByLevel l ss
    s = sum . map avgGrade $ fs

filterByLevel :: Level -> [Student] -> [Student]
filterByLevel l = filter ((==l) . level)

avgGradePerLevels :: [Student] -> (Double, Double, Double)
avgGradePerLevels ss = (getAvg Bachelor ss, getAvg Master ss, getAvg PhD ss)

-- 2.3
-- Write a function that returns a list of matriculation numbers for a given
-- study level, sorted by average grade in descending order.
-- rankedStudents :: Level -> [Students] -> [String]

rankedStudents :: Level -> [Student] -> [String]
rankedStudents l = map studentId . sortBy descendingOrder . filterByLevel l
  where
    descendingOrder (Student {avgGrade=avg1}) (Student {avgGrade=avg2}) = compare avg2 avg1

-- 2.4
-- Write a function
-- addStudent :: Student -> [Student] -> [Student]
-- that adds a student to a list of students. If a student with an identical
-- matriculation number already exists in the list, the function should
-- return an error.

addStudent :: Student -> [Student] -> [Student]
addStudent s ss
  | studentId s `elem` map studentId ss = error "Student already exists"
  | otherwise = s:ss

-- EXERCISE 3 ======================================

-- 3.1.
-- Define your own parametrized type 'MyTriplet' that contains the values of
-- three different types. Do this using a record.
-- Define a function
-- toTriplet :: MyTriplet a b c -> (a,b,c)
-- that converts a 'MyTriplet' value into an ordinary triplet.

data MyTriplet a b c = MyTriplet { af :: a, bf :: b, cf :: c }

toTriplet :: MyTriplet a b c -> (a,b,c)
toTriplet (MyTriplet a b c) = (a,b,c)
toTriplet' :: MyTriplet a b c -> (a,b,c)
toTriplet' t = (af t, bf t, cf t)

-- 3.2.
-- Define a function
-- totalSalaries :: [Employee] -> Double
-- that sums the known salaries of employees (salaries that are not
-- 'Nothing').

data Employee = Employee
  { name   :: String
  , salary :: Maybe Double } deriving Show

totalSalaries :: [Employee] -> Double
totalSalaries = sum . map (getSalary . salary)
  where
    getSalary Nothing = 0
    getSalary (Just x) = x

-- 3.3
-- Write a function 'addStudent2' that works like 'addStudent' from problem 2.4
-- but returns a 'Maybe' type instead of an error.
-- addStudent2 :: Student -> [Student] -> Maybe [Student]
-- Write 'addStudent3' that returns an 'Either'.

addStudent2 :: Student -> [Student] -> Maybe [Student]
addStudent2 s ss
  | studentId s `elem` map studentId ss = Nothing
  | otherwise = Just $ s:ss

addStudent3 :: Student -> [Student] -> Either String [Student]
addStudent3 s ss
  | studentId s `elem` map studentId ss = Left "Student already exists"
  | otherwise = Right $ s:ss
