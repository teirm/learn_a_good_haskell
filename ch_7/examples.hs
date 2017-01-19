module Shapes
( Point(..) -- Exports all value constructors for given type
, Shape(..)
, area
, nudge
, baseCircle
, baseRect
) where 

-- The type is Point
-- The value constructor is Point
data Point = Point Float Float deriving (Show)

-- The type is Shape
-- The value constructors are Circle and Rectangle
data Shape = Circle Point Float | Rectangle Point Point 
    deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b
    = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 +a) (y2+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- Car data type
data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

-- Vector data type
-- Type class constraint enforced in functions
-- Not data declarations
data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

-- Inheriting from Type Classes
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Before Data.map
phoneBook :: [(String, String)]
phoneBook = 
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2429")
    ]

-- Recursive Data Structures
-- left associative fixity 5 :-:
-- Number is tightness of binding
infixr 5 -++
(-++) :: [a] -> [a] -> [a]
[]      -++ ys = ys
(x:xs)  -++ ys = x : (xs -++ ys)

infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)
