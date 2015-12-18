module Shape where

{- 2 shapes, circles and squares. 2 operations, area and perimeter -}

data Length = MkLength Float deriving (Eq, Show)
data Radius = MkRadius Float deriving (Eq, Show)
{-
-- data inroduces a new type, type introduces a type synonym 
data Shape = Square Length
	   | Circle Radius
	   deriving (Eq, Show)

myRadius :: Radius
myRadius = MkRadius 3.0
-}

type Length = Float
type Radius = Float

data Shape = Square Length
	| Circle Radius
	deriving (Eq, Show)

-- compute the area of a square.
-- compute the area of a circle.

area :: Shape -> Float
area (Square l) = l * l
area (Circle r) = pi * r * r


-- compute the perimeter of a square.
-- compute the perimeter of a circle.

perimeter :: Shape -> Float
perimeter (Square l) = 4 * l
perimeter (Circle r) = 2 * pi * r
