{-# LANGUAGE InstanceSigs #-}

module Geometry.Transformers
  ( ShapeTransformer(..)
  , scale
  , changeColor
  , translate
  , rotate
  , logTransform
  , simpleChain
  , complexTransformation
  ) where

import Geometry.Core

-- ShapeTransformer et instances
newtype ShapeTransformer a = ShapeTransformer 
  { runTransform :: ColoredShape -> (ColoredShape, a) 
  }

instance Functor ShapeTransformer where
  fmap :: (a -> b) -> ShapeTransformer a -> ShapeTransformer b
  fmap f transformer = ShapeTransformer $ \cs ->
    let (newShape, result) = runTransform transformer cs
    in (newShape, f result)

instance Applicative ShapeTransformer where
  pure :: a -> ShapeTransformer a
  pure x = ShapeTransformer $ \shape -> (shape, x)
  
  (<*>) :: ShapeTransformer (a -> b) -> ShapeTransformer a -> ShapeTransformer b
  stf <*> stx = ShapeTransformer $ \initialShape ->
    let (shape1, f) = runTransform stf initialShape
        (shape2, x) = runTransform stx shape1
    in (shape2, f x)

-- Transformations de base
scale :: Float -> ShapeTransformer Float
scale factor = ShapeTransformer $ \cs ->
  let scaledShape = scaleShape (shape cs) factor
  in (cs { shape = scaledShape }, factor)
  where
    scaleShape (Circle r) f = Circle (r * f)
    scaleShape (Rectangle w h) f = Rectangle (w * f) (h * f)
    scaleShape (Triangle a b c) f = Triangle (a * f) (b * f) (c * f)

changeColor :: Color -> ShapeTransformer Color
changeColor newColor = ShapeTransformer $ \cs ->
  (cs { color = newColor }, color cs)  -- retourne l'ancienne couleur

translate :: Float -> Float -> ShapeTransformer (Float, Float)
translate dx dy = ShapeTransformer $ \cs ->
  let newName = name cs ++ "_translated_" ++ show dx ++ "_" ++ show dy
  in (cs { name = newName }, (dx, dy))

rotate :: Float -> ShapeTransformer Float
rotate angle = ShapeTransformer $ \cs ->
  let newName = name cs ++ "_rotated_" ++ show angle
  in (cs { name = newName }, angle)

-- Fonctions utilitaires pour transformations
logTransform :: String -> ShapeTransformer a -> ShapeTransformer (String, a)
logTransform message transformer = 
  fmap (\result -> (message, result)) transformer

-- Chaines de transformations
simpleChain :: ShapeTransformer (String, Float)
simpleChain =
  pure (\scaleResult colorResult -> ("Simple chain", scaleResult))
  <*> scale 2.0
  <*> changeColor Red

complexTransformation :: ShapeTransformer (String, Float, Color, (Float, Float), Float)
complexTransformation = 
  let scaleOp = logTransform "Mise a l'echelle" (scale 1.5)
      colorOp = changeColor Green
      moveOp = logTransform "Translation" (translate 5.0 3.0)
      rotateOp = rotate 45.0
  in pure (\scaleLog colorResult moveLog rotateResult -> 
              (fst scaleLog ++ "; " ++ fst moveLog,
               snd scaleLog,
               colorResult, 
               snd moveLog,
               rotateResult))
     <*> scaleOp
     <*> colorOp
     <*> moveOp
     <*> rotateOp