module Geometry.Core
  ( Shape(..)
  , Color(..)
  , ColoredShape(..)
  , BoundingBox(..)
  , perimeter
  , area
  , boundingBox
  ) where

-- Types de données de base
data Shape = 
    Circle Float              -- rayon
  | Rectangle Float Float     -- longueur et hauteur  
  | Triangle Float Float Float  -- trois cotes
  deriving (Show, Eq)

data Color = Red | Green | Blue | Yellow | Black
  deriving (Show, Eq, Enum, Bounded)

data ColoredShape = ColoredShape 
  { shape :: Shape
  , color :: Color
  , name :: String
  }
  deriving (Show, Eq)

data BoundingBox = BoundingBox
  { width  :: Float  -- largeur maximale
  , height :: Float  -- hauteur maximale
  }
  deriving (Show, Eq)

-- Fonctions de calcul de base
perimeter :: Shape -> Float
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle l h) = (l + h) * 2
perimeter (Triangle a b c) = a + b + c

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle l h) = l * h
area (Triangle a b c) = 
  let s = (a + b + c) / 2  -- demi-perimetre
  in sqrt (s * (s - a) * (s - b) * (s - c))

-- Calcul du bounding box pour une forme
boundingBox :: Shape -> BoundingBox
boundingBox (Circle r)       = BoundingBox (2*r) (2*r)
boundingBox (Rectangle w h)  = BoundingBox w h
boundingBox (Triangle a b c) = 
  let sortedSides = reverse (quicksort [a, b, c])  -- deux plus grands cotes
  in case sortedSides of
        (x:y:_) -> BoundingBox x y
        _ -> BoundingBox 0 0  -- cas par defaut

-- Fonction utilitaire locale
quicksort :: [Float] -> [Float]
quicksort [] = []
quicksort (x:xs) = 
  quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]