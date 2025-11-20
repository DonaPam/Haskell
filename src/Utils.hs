module Utils
  ( createSampleShapes
  , printShapeGroup
  ) where

import Geometry.Core
import Geometry.Groups

-- Donnees de test
createSampleShapes :: [ColoredShape]
createSampleShapes =
  [ ColoredShape 
      { shape = Circle 5.0
      , color = Red
      , name = "Cercle1"
      }
  , ColoredShape
      { shape = Rectangle 8.0 6.0
      , color = Green
      , name = "Rectangle1"
      }
  , ColoredShape
      { shape = Triangle 3.0 4.0 5.0
      , color = Blue
      , name = "Triangle1"
      }
  ]

-- Fonctions d'affichage
printShapeGroup :: ShapeGroup -> IO ()
printShapeGroup sg = do
  putStrLn $ "  BoundingBox: " ++ show (groupBBox sg)
  putStrLn $ "  Perimetre total: " ++ show (totalPerimeter sg)
  putStrLn $ "  Nombre de formes: " ++ show (shapeCount sg)
  putStrLn $ "  Noms: " ++ show (shapeNames sg)