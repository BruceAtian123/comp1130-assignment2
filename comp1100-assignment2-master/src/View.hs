--- Copyright 2018 The Australian National University, All rights reserved
module View where

import CodeWorld hiding (Point)
import Data.Text (pack)
import Data.List
import Model



-- a pixel coordinate is a pair of Int
type Coord = (Int, Int)

-- a pixel value is some shade of grey (0.0 == white, 1.0 = black)
type Shade = Double

-- a raster pixel is represented by a coordinate and a shade
type Pixel = (Coord, Shade)

-- a raster is a list of pixels
type Raster = [Pixel]

-- $setup
-- >>> import Data.List (sort)

coordToPoint :: Resolution -> Coord -> Point
coordToPoint z (x, y) = (x', y')
  where
    x' = fromIntegral x * z
    y' = fromIntegral y * z

pointToCoord :: Resolution -> Point -> Coord
pointToCoord z (x, y) = (x', y')
  where
    x' = round $ x / z
    y' = round $ y / z

makeView :: Maybe Tool -> Raster -> Resolution -> Picture
makeView t rs z =
  coordinatePlane &
  pictures (map pixelToPicture rs) &
  translated (-13.5) 8 $
  (text . pack) ("Shape: " ++ shapeToText)
  where
    shapeToText = take (length shape - 4) shape
    shape = takeWhile (/= ' ') $ maybe "" show t
    pixelToPicture (c, b) = translated x' y' p
      where
        p = coloured (grey $ 1 - b) $ solidRectangle z z
        (x', y') = coordToPoint z c


-- Update the view based on the model by constructing a rasterised CodeWorld picture
updateView :: Model -> Picture
updateView (Model ss z s t (False, _, _)) =
  makeView t (concatMap (shapeToRaster z s) ss) z

updateView (Model _ z _ t (True, _, d)) =
  makeView t d z

-- Construct a raster for a shape at the given resolution (optionally smoothed)
shapeToRaster :: Resolution -> Smooth -> Shape -> Raster
shapeToRaster z s shape =
  case shape of
    Point p1 -> pointRaster $ pointToCoord z p1
    Rectangle p1 p2 -> rectangleRaster (pointToCoord z p1) (pointToCoord z p2)
    Line p1 p2 -> lineRaster s (pointToCoord z p1) (pointToCoord z p2)
    Polygon pts -> polyLineRaster s (foldl (\list e -> pointToCoord z e:list) [] pts)
    Circle p1 p2 -> circleRaster s (pointToCoord z p1) (pointToCoord z p2)

-- | A raster for the point p
-- Examples:
-- >>> pointRaster (1,1)
-- [((1,1),1.0)]
pointRaster :: Coord -> Raster
pointRaster p = [(p, 1)]

-- | A raster for the rectangle with corner coordinates (x1,y1) and (x2,y2)
-- Examples:
-- >>> sort $ rectangleRaster (-1,-1) (1,1)
-- [((-1,-1),1.0),((-1,0),1.0),((-1,1),1.0),((0,-1),1.0),((0,1),1.0),((1,-1),1.0),((1,0),1.0),((1,1),1.0)]

rectangleRaster :: Coord -> Coord -> Raster
rectangleRaster (a,b) (c,d) =
  let ((left, bottom), (right, top)) = ((min a c, min b d), (max a c, max b d))
      lb = (left, bottom)
      lt = (left, top)
      rb = (right, bottom)
      rt = (right, top) in
       nub([(lb, 1.0), (lt, 1.0), (rb, 1.0), (rt, 1.0)] ++
       init (drop 1 $ lineRaster False lb rb) ++
       init (drop 1 $ lineRaster False lt rt) ++
       init (drop 1 $ lineRaster False lb lt) ++
       init (drop 1 $ lineRaster False rb rt))



-- | A raster for the line with end coordinates given as arguments.
-- Antialias if smooth is true.
-- Examples:
-- >>> sort $ lineRaster False (-1,-1) (1,1)
-- [((-1,-1),1.0),((0,0),1.0),((1,1),1.0)]
--
-- prop> a == (fst $ head $ lineRaster False a b)
-- prop> b == (fst $ last $ lineRaster False a b)

linearToCoord :: Int -> Int -> [Int]
linearToCoord a b = if a > b then [b..a] else [a..b]

linearToPoint :: Int -> Int -> [Double]
linearToPoint a b = fromIntegral <$> linearToCoord a b

to :: Int -> Int -> [Int]
to a b = if a > b then reverse [b..a] else [a..b]

plot :: Int -> Int -> Double -> Raster
plot a b c = [((a, b), c)]

ipart :: Double -> Int
ipart = floor

-- round is just round

fpart :: Double -> Double
fpart x = x - fromIntegral (floor x :: Integer)

rfpart :: Double -> Double
rfpart x = 1.0 - fpart x

firstPoint :: Point -> Point -> Double -> Bool -> Raster
firstPoint (x0, y0) _ gradient steep =
  let xend = fromIntegral (round x0 :: Integer) :: Double
      yend = y0 + gradient * (xend - x0)
      xgap = rfpart (x0 + 0.5)
      xpxl1 = round xend
      ypxl1 = ipart yend in
     if steep then
        plot ypxl1       xpxl1 (rfpart yend * xgap) ++
        plot (ypxl1 + 1) xpxl1 (fpart yend * xgap)
      else
        plot xpxl1 ypxl1       (rfpart yend * xgap) ++
        plot xpxl1 (ypxl1 + 1) ( fpart yend * xgap)

secondPoint :: Point -> Point -> Double -> Bool -> Raster
secondPoint _ (x1, y1) gradient steep =
  let xend  = fromIntegral (round x1 :: Integer) :: Double
      yend  = y1 + gradient * (xend - x1)
      xgap  = fpart (x1 + 0.5)
      xpxl2 = round xend
      ypxl2 = ipart yend in
     if steep then
        plot ypxl2       xpxl2 (rfpart yend * xgap) ++
        plot (ypxl2 + 1) xpxl2 (fpart yend * xgap)
      else
        plot xpxl2 ypxl2       (rfpart yend * xgap) ++
        plot xpxl2 (ypxl2 + 1) ( fpart yend * xgap)

singlePoint :: Int -> Double -> Bool -> Raster
singlePoint x intery steep =
  if steep then
    plot (ipart intery)         x (rfpart intery) ++
    plot (succ $ ipart intery)  x ( fpart intery)
  else
    plot x (ipart intery)       (rfpart intery) ++
    plot x (succ $ ipart intery) (fpart intery)

remainPoints :: Point -> Point -> Double -> Bool -> Raster
remainPoints (x0, y0) (x1, _) gradient steep =
  let yend   = y0 + gradient * (fromIntegral (round x0 :: Integer) - x0)
      intery = yend + gradient
      xpxl1  = round x0
      xpxl2  = round x1 in
      concat [ singlePoint x iy steep
              | i <- [0 .. xpxl2 - xpxl1 - 2],
                let x  = i + xpxl1 + 1,
                let iy = intery + gradient * fromIntegral i
              ]

drawLineWu :: Point -> Point -> Raster
drawLineWu (x0'', y0'') (x1'', y1'') =
  let steep = abs(y1'' - y0'') > abs(x1'' - x0'')
      (x0', y0', x1', y1') = if steep then (y0'', x0'', y1'', x1'') else (x0'', y0'', x1'', y1'')
      (x0, y0, x1, y1) = if x0' > x1' then (x1', y1', x0', y0') else (x0', y0', x1', y1')
      dx = x1 - x0
      dy = y1 - y0
      g = dy / dx
      gradient = if g == 0.0 then 1.0 else g
      p1 = firstPoint (x0, y0) (x1, y1) gradient steep
      p2 = secondPoint (x0, y0) (x1, y1) gradient steep
      remain = remainPoints (x0, y0) (x1, y1) gradient steep
      in
      p1 ++ p2 ++ remain





lineRaster :: Smooth -> Coord -> Coord -> Raster
lineRaster True (x1, y1) (x2, y2) = drawLineWu (fromIntegral x1, fromIntegral y1) (fromIntegral x2,fromIntegral y2)
lineRaster False  (x1, y1) (x2, y2) = nub [((x1+x, y1+y),1) | (x,y) <- bres (x2-x1) (y2-y1)]

bres :: Int -> Int -> [(Int,Int)]
bres run rise
    | run  <  0  = [(-x, y) | (x, y) <- bres (-run) rise]
    | rise <  0  = [(x, -y) | (x, y) <- bres run (-rise)]
    | rise > run = [(x,  y) | (y, x) <- bres rise run   ]
    | otherwise  = zip [0..run] (fst <$> iterate step (0, run `div` 2))
    where
        step (y, error'')
            | error' < 0 = (y + 1, error' + run)
            | otherwise  = (y,     error')
            where error' = error'' - rise

-- | A raster for the polyline with vertices vs.
-- Antialias if smooth is true.
-- Examples:
-- >>> sort $ polyLineRaster False [(0,0),(2,2)]
-- [((0,0),1.0),((1,1),1.0),((2,2),1.0)]
-- >>> sort $ polyLineRaster False [(0,0),(1,1),(0,1)]
-- [((0,0),1.0),((0,1),1.0),((1,1),1.0)]
--
-- prop> lineRaster False a b == polyLineRaster False [a, b]

polyLineRaster :: Smooth -> [Coord] -> Raster
polyLineRaster smooth pts = nub(foldl (\line a->uncurry (lineRaster smooth) a ++ line) [] side)
       where side
                | length pts == 2 = [(head pts, pts !! 1)]
                | otherwise = zip pts (drop 1 pts ++ pts)

toY :: Double -> Int -> Double
toY r x = sqrt (r * r - fromIntegral x * fromIntegral x)
-- | A raster for the circle with center (x1,y1) and intersecting (x2,y2)
-- Antialias if smooth is true.
-- Examples:
-- >>> sort $ circleRaster False (0,0) (0,1)
-- [((-1,0),1.0),((0,-1),1.0),((0,1),1.0),((1,0),1.0)]

dc :: Double -> Double -> Double
dc r y = let x = sqrt (r * r - y * y) in fromIntegral (ceiling x :: Integer) - x


circleRaster :: Smooth -> Coord -> Coord -> Raster
circleRaster True (x1, y1) (x2, y2) =
  let r =  sqrt (fromIntegral (x1 - x2) ** 2.0 + fromIntegral (y1 - y2) ** 2.0)
      r :: Double
      x :: Double
      y :: Double
      d :: Double
      (x, y, d) = (r, 0.0, 0.0)
      p1     = plot (round x) (round y) 1
      result = snd <$> takeWhile (\((cx, cy, _), _) -> cx > cy) (iterate step ((x, y, d), p1))
      step ((cx, cy, cd), pts) =
        let newy = cy + 1.0
            newx = if dc r newy < cd then cx - 1.0 else cx
            newd = dc r newy
            points = plot (round newx) (round newy) (1.0 - newd) ++ plot (round (newx - 1)) (round newy) newd in
            ((newx, newy, newd), points ++ pts)
      in
        concat [ [((x1 + cx, y1 + cy), a),
                  ((x1 + cy, y1 + cx), a),
                  ((x1 - cx, y1 + cy), a),
                  ((x1 - cy, y1 + cx), a),
                  ((x1 + cx, y1 - cy), a),
                  ((x1 + cy, y1 - cx), a),
                  ((x1 - cx, y1 - cy), a),
                  ((x1 - cy, y1 - cx), a)]
                  | ((cx, cy), a) <- concat result]

circleRaster False (x1, y1) (x2, y2) =
  let r = sqrt (fromIntegral (x1 - x2) ** 2.0 + fromIntegral (y1 - y2) ** 2.0)
      r :: Double
      initstate = (0, round r, round (1-r))
      coords = takeWhile (\(x,y,_) -> y >= x) $
               iterate (\(x,y,d) ->
                  if d < 0 then
                    (x+1, y, d+2*x+3)
                  else
                    (x+1, y-1, d+2*(x-y)+5)
                ) initstate
                in
                nub(foldl (\cds (cx, cy, _) ->
                  ((x1 + cy, y1 + cx), 1.0):
                  ((x1 - cy, y1 + cx), 1.0):
                  ((x1 - cy, y1 - cx), 1.0):
                  ((x1 + cy, y1 - cx), 1.0):
                  ((x1 + cx, y1 + cy), 1.0):
                  ((x1 - cx, y1 - cy), 1.0):
                  ((x1 + cx, y1 - cy), 1.0):
                  ((x1 - cx, y1 + cy), 1.0):
                  cds
                ) [] coords)
