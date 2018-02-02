-- - Define a 'Date' structure with the appropriate fields.
-- - Define a function that shows a date in the DD.MM.YYYY format (without
--   leading zeroes).
import Data.List

data Date = Date Int Int Int

showDate :: Date -> String
showDate (Date d m y) = show d ++ "." ++ show m ++ "." ++ show y

-- - Define a function
-- 
-- that translates a shape into the direction of vector (x,y).

data Point = Point Double Double 
  deriving Show
data Shape2 = Circle2 Point Double | Rectangle2 Point Point 
  deriving Show

translate :: Point -> Shape2 -> Shape2
translate (Point x y) (Circle2 (Point x2 y2) r) = Circle2 (Point (x2 + x) (y2 + y)) r
translate (Point x y) (Rectangle2 (Point x2 y2) (Point x3 y3)) = Rectangle2 (Point (x2 + x) (y2 + y)) (Point (x3 + x) (y3 + y))

data Level   = Bachelor | Master | PhD deriving (Show, Eq)
data Student = Student
 { firstName  :: String
 , lastName   :: String
 , studentId  :: String
 , level      :: Level
 , avgGrade   :: Double } deriving Show


-- - Define a function that increases the average grade of the student by 1.0,
-- but not above 5.0.
improveStudent :: Student -> Student
improveStudent s = s { avgGrade = min 5.0 (avgGrade s + 1) }

-- - Write a function to compute the average grade of students for the different
-- study levels.
avgGradePerLevels :: [Student] -> (Double, Double, Double)
