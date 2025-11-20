module Lib
  ( module Geometry.Core
  , module Geometry.Groups
  , module Geometry.Transformers
  , module Utils
  , demo
  ) where

-- Reexportation de tous les modules
import Geometry.Core
import Geometry.Groups
import Geometry.Transformers
import Utils

-- Demonstration principale
demo :: IO ()
demo = do
  putStrLn "=== Demonstration du systeme de traitement de formes geometriques ==="
  
  let shapes = createSampleShapes
  
  putStrLn "\n1. Formes initiales:"
  mapM_ print shapes
  
  putStrLn "\n2. ShapeGroup et Monoid:"
  let group = combineShapes shapes
  printShapeGroup group
  
  putStrLn "\n3. Test Functor:"
  let shape = head shapes
  let scaledWithLog = logTransform "Mise a l'echelle" (scale 1.5)
  let (shape2, (logMsg, factor2)) = runTransform scaledWithLog shape
  putStrLn $ "Log: " ++ logMsg
  putStrLn $ "Facteur: " ++ show factor2
  
  putStrLn "\n4. Test Applicative:"
  let (shapeSimple, (simpleLog, scaleFactor)) = runTransform simpleChain shape
  putStrLn $ "Chaine simple - Log: " ++ simpleLog
  putStrLn $ "Chaine simple - Echelle: " ++ show scaleFactor
  
  putStrLn "\n5. Demonstration terminee"