--- Copyright 2018 The Australian National University, All rights reserved
module Model where

type Point = (Double, Double)

data Shape
  = Point Point
  | Rectangle Point
              Point
  | Circle Point
           Point
  | Line Point
         Point
  | Polygon [Point]
  deriving (Show)

type Shapes = [Shape]

type Resolution = Double

type Smooth = Bool

type RasterData = ((Int, Int), Double)
data Tool
  = PointTool (Maybe Point)
  | RectangleTool (Maybe Point)
  | LineTool (Maybe Point)
  | CircleTool (Maybe Point)
  | PolygonTool [Point]
  deriving (Show)

type Time = Double
type Animation = (Bool, Time, [RasterData])
data Model =
  Model Shapes
        Resolution
        Smooth
        (Maybe Tool)
        Animation
  deriving (Show)

initialModel :: Model
initialModel = Model [] 1 False Nothing (False, 0.0, [])
