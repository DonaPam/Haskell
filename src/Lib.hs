{-# LANGUAGE InstanceSigs #-}

module Lib
  ( demo
  , createSampleShapes
  , Shape(..)
  , Color(..)
  , ColoredShape(..)
  , BoundingBox(..)
  , ShapeGroup(..)
  , ShapeTransformer(..)
  , perimeter
  , area
  ) where

import Geometry.Core
import Geometry.Groups
import Geometry.Transformers

-- Тестовые данные
createSampleShapes :: [ColoredShape]
createSampleShapes =
  [ ColoredShape (Circle 5.0) Red "Circle1"
  , ColoredShape (Rectangle 8.0 6.0) Green "Rectangle1"
  , ColoredShape (Triangle 3.0 4.0 5.0) Blue "Triangle1"
  ]

-- Функция для красивого вывода ShapeGroup
printShapeGroup :: ShapeGroup -> IO ()
printShapeGroup sg = do
  putStrLn $ "  BoundingBox: " ++ show (groupBBox sg)
  putStrLn $ "  Периметр: " ++ show (totalPerimeter sg)
  putStrLn $ "  Количество фигур: " ++ show (shapeCount sg)
  putStrLn $ "  Имена: " ++ show (shapeNames sg)

-- Основная демонстрационная функция
demo :: IO ()
demo = do
  putStrLn " Демонстрация системы обработки геометрических фигур "
  
  let shapes = createSampleShapes
  
  putStrLn "\n1. Исходные фигуры:"
  mapM_ print shapes
  
  putStrLn "\n2. Bounding boxes отдельных фигур:"
  mapM_ (\cs -> putStrLn $ "  " ++ name cs ++ ": " ++ show (boundingBox (shape cs))) shapes
  
  putStrLn "\n3. ShapeGroup и Monoid:"
  let group = combineShapes shapes
  printShapeGroup group
  
  putStrLn "\n4. Тест нейтрального элемента (mempty):"
  let emptyGroup = mempty :: ShapeGroup
  printShapeGroup emptyGroup
  
  putStrLn "\n5. Тест комбинации групп:"
  let group1 = shapeGroupFromColored (shapes !! 0)
  let group2 = shapeGroupFromColored (shapes !! 1)
  let combinedGroup = group1 <> group2
  putStrLn "Группа 1 (Circle):"
  printShapeGroup group1
  putStrLn "Группа 2 (Rectangle):"
  printShapeGroup group2
  putStrLn "Объединенная группа:"
  printShapeGroup combinedGroup
  
  putStrLn "\n6. Проверка законов Monoid:"
  putStrLn $ "group1 <> mempty == group1: " ++ show (group1 <> mempty == group1)
  putStrLn $ "mempty <> group1 == group1: " ++ show (mempty <> group1 == group1)

  putStrLn "\n ЧАСТЬ 2: Functor "
  
  case shapes of
    [] -> error "The list can't be empty"
    (firstShape:_) -> do
      putStrLn $ "Transformation of: " ++ show firstShape
  
      putStrLn "\n1. Простое масштабирование:"
      let (shape1, factor) = runTransform (scale 2.0) firstShape
      putStrLn $ "Коэффициент: " ++ show factor
      putStrLn $ "Результат: " ++ show shape1
    
      putStrLn "\n2. Масштабирование с логированием (fmap):"
      let scaledWithLog = logTransform "Масштабирование применено" (scale 1.5)
      let (shape2, (logMsg, factor2)) = runTransform scaledWithLog firstShape
      putStrLn $ "Лог: " ++ logMsg
      putStrLn $ "Коэффициент: " ++ show factor2
      putStrLn $ "Результат: " ++ show shape2
    
      putStrLn "\n3. Изменение цвета:"
      let (shape3, oldColor) = runTransform (changeColor Blue) firstShape
      putStrLn $ "Старый цвет: " ++ show oldColor
      putStrLn $ "Новый цвет: " ++ show (color shape3)

      putStrLn "\n ЧАСТЬ 3: Applicative "
    
      putStrLn "\n1. Тест функции pure:"
      let (shapePure, value) = runTransform (pure "Nouveau") firstShape
      putStrLn $ "Значение: " ++ show value
      putStrLn $ "Фигура не изменилась: " ++ show (shapePure == firstShape)
    
      putStrLn "\n2. Простая цепочка с Applicative:"
      let (shapeSimple, (simpleLog, scaleFactor)) = runTransform simpleChain firstShape
      putStrLn $ "Лог: " ++ simpleLog
      putStrLn $ "Коэффициент масштабирования: " ++ show scaleFactor
      putStrLn $ "Результат: " ++ show shapeSimple
    
      putStrLn "\n3. Комплексная цепочка с Applicative:"
      let (shapeComplex, (fullLog, scaleFactor2, newColor, moveParams, rotateAngle)) = 
            runTransform complexTransformation firstShape
      putStrLn $ "Полный лог: " ++ fullLog
      putStrLn $ "Масштаб: " ++ show scaleFactor2
      putStrLn $ "Новый цвет: " ++ show newColor
      putStrLn $ "Перемещение: " ++ show moveParams
      putStrLn $ "Вращение: " ++ show rotateAngle
      putStrLn $ "Финальная фигура: " ++ show shapeComplex
    
      putStrLn "\n4. Проверка законов Applicative:"
      -- Идентичность: pure id <*> v = v
      let identityLaw = runTransform (pure id <*> scale 2.0) firstShape
      putStrLn $ "Закон идентичности выполнен: " ++ show (snd identityLaw == 2.0)
  
  putStrLn "\n Демонстрация завершена "