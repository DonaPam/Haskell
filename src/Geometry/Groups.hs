{-# LANGUAGE InstanceSigs #-}

module Geometry.Groups
  ( ShapeGroup(..)
  , shapeGroupFromColored
  , combineShapes
  , combineBBox
  ) where

import Geometry.Core

-- ShapeGroup et instances Monoid
data ShapeGroup = ShapeGroup
  { groupBBox :: BoundingBox
  , totalPerimeter :: Float
  , shapeCount :: Int
  , shapeNames :: [String]
  }
  deriving (Show, Eq)

-- Combinaison de bounding boxes
combineBBox :: BoundingBox -> BoundingBox -> BoundingBox
combineBBox (BoundingBox w1 h1) (BoundingBox w2 h2) =
  BoundingBox (max w1 w2) (max h1 h2)

-- Conversion d'une forme coloree en groupe
shapeGroupFromColored :: ColoredShape -> ShapeGroup
shapeGroupFromColored cs = ShapeGroup
  { groupBBox = boundingBox (shape cs)
  , totalPerimeter = perimeter (shape cs)
  , shapeCount = 1
  , shapeNames = [name cs]
  }

-- Combinaison de formes
combineShapes :: [ColoredShape] -> ShapeGroup
combineShapes = foldMap shapeGroupFromColored

-- Instances
instance Semigroup ShapeGroup where
  (<>) :: ShapeGroup -> ShapeGroup -> ShapeGroup
  sg1 <> sg2 = ShapeGroup
    { groupBBox = combineBBox (groupBBox sg1) (groupBBox sg2)
    , totalPerimeter = totalPerimeter sg1 + totalPerimeter sg2
    , shapeCount = shapeCount sg1 + shapeCount sg2  
    , shapeNames = shapeNames sg1 ++ shapeNames sg2
    }

instance Monoid ShapeGroup where
  mempty :: ShapeGroup
  mempty = ShapeGroup
    { groupBBox = BoundingBox 0 0
    , totalPerimeter = 0
    , shapeCount = 0
    , shapeNames = []
    }