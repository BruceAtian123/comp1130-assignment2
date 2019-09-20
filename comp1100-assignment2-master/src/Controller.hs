--- Copyright 2018 The Australian National University, All rights reserved
module Controller where

import CodeWorld hiding (Point)
import Model
import View
import Data.Char (isDigit)
import Data.List (sort, group)
import Data.Text (pack, unpack)

conwayArray :: [(Int, Int)]
conwayArray = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]



conway :: Resolution -> Raster -> Raster
conway z arr = 
  let live = map fst arr
      points = [  (dx + cx, dy + cy)  | (dx, dy) <- conwayArray, (cx, cy) <- live ]
      newpoints = group $ sort points
      p = head <$> filter (\l -> length l == 3) newpoints
      q = head <$> filter (\l -> length l == 2 && head l `elem` live) newpoints
     in
      [((x, y), 1.0) | (x, y) <- p ++ q, 
                            x >= round(-10 / z),
                            x <= round(10 / z), 
                            y >= round(-10 / z),
                            y <= round(10 / z)]
      


handleTime :: Double -> Model -> Model
handleTime newtime (Model ss z s t (running, time, d)) =
  if not running || newtime + time < 0.4 then
    Model ss z s t (running, newtime + time, d)
  else
    Model ss z s t (running, 0, conway z d)

getInitAnimationState :: Model -> Model
getInitAnimationState (Model ss z s t (running, time, _)) =
  Model ss z s t (running, time, concatMap (shapeToRaster z s) ss)



handleEvent :: Event -> Model -> Model
handleEvent event m@(Model ss z s t a@(running, _, _)) =
  case event of
    KeyPress key
      | k == "Esc" -> initialModel
        -- revert to an empty canvas
      | k == "D" -> trace (pack $ show m) m
        --   the current model on the console
      | k == "S" -> Model ss z (not s) t a
        -- turn smoothing on/off
      | k == "0" -> Model ss (1 / 10) s t a
        -- set the pixel resolution to 0.1
      | isDigit . head $ k -> Model ss (1 / read [head k]) s t a
        -- set the pixel resolution to 1/k
      | k == "-" || k == "," -> Model ss (z / 2.0) s t a
        -- halve the pixel resolution
      | k == "=" || k == "." -> Model ss (z * 2.0) s t a
        -- double the pixel resolution
      | k == "Backspace" || k == "Delete" -> Model (drop 1 ss) z s t a
        -- drop the last added shape
      | k == "G" ->
          if not running then
            if not (null ss) then
              getInitAnimationState $ Model ss z False t (True, 0.0, [])
            else
              m
          else
            Model [] z False t (False, 0.0, [])
      | k == " " ->
        case t of
          Just (PolygonTool l@(_:_:_:_)) -- polygons have at least three vertices
           -> Model (Polygon l : ss) z s Nothing a
              -- add a polygon
          _ -> m -- not enough vertices yet
      | otherwise -> maybe m (\tool -> Model ss z s (Just tool) a) (selectTool k)
      where k = unpack key
    MousePress btn p
      | btn == LeftButton -> maybe m (\tool -> Model ss z s (Just tool) a ) (updateTool t p)
      | otherwise -> m
    MouseRelease btn p
      | btn == LeftButton ->
        maybe m (\shape -> Model (shape : ss) z s Nothing a) (triggerTool t p)
      | otherwise -> m
    _ -> m

selectTool :: String -> Maybe Tool
selectTool k =
  case k of
    "O" -> Just $ PointTool Nothing
    "R" -> Just $ RectangleTool Nothing
    "L" -> Just $ LineTool Nothing
    "C" -> Just $ CircleTool Nothing
    "P" -> Just $ PolygonTool []
    _ -> Nothing

updateTool :: Maybe Tool -> Point -> Maybe Tool
updateTool t p =
  case t of
    Just (PointTool _) -> Just $ PointTool $ Just p
    Just (PolygonTool vs) -> Just $ PolygonTool $ p : vs
    Just (RectangleTool _) -> Just $ RectangleTool $ Just p
    Just (LineTool _) -> Just $ LineTool $ Just p
    Just (CircleTool _) -> Just $ CircleTool $ Just p
    _ -> t

triggerTool :: Maybe Tool -> Point -> Maybe Shape
triggerTool t p2 =
  case t of
    Just (PolygonTool _) -> Nothing
    Just (PointTool (Just p1)) -> Just $ Point p1
    Just (RectangleTool (Just p1)) -> Just $ Rectangle p1 p2
    Just (LineTool (Just p1)) -> Just $ Line p1 p2
    Just (CircleTool (Just p1)) -> Just $ Circle p1 p2
    _ -> Nothing
