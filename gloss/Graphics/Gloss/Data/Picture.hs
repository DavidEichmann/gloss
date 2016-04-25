
module Graphics.Gloss.Data.Picture
        ( Picture       (..)
        , Point, Vector, Path

        -- * Aliases for Picture constructors
        , blank
        , polygon
        , line
        , circle, thickCircle
        , arc,    thickArc
        , text
        , bitmap
        , color
        , translate, rotate, scale
        , pictures

        -- * Compound shapes
        , lineLoop
        , circleSolid
        , arcSolid
        , sectorWire
        , rectanglePath
        , rectangleWire
        , rectangleSolid
        , rectangleUpperPath
        , rectangleUpperWire
        , rectangleUpperSolid

        , isPointInTriangle)
where
import Graphics.Gloss.Rendering
import Graphics.Gloss.Geometry.Angle
import Data.List

import Control.Monad.ST
import Data.STRef
import Control.Monad.Fix

import Debug.Trace

-- Constructors ----------------------------------------------------------------
-- NOTE: The docs here should be identical to the ones on the constructors.

-- | A blank picture, with nothing in it.
blank :: Picture
blank   = Blank

-- | A convex polygon filled with a solid color.
polygon :: Path -> Picture
polygon = generalPolygon

-- | A line along an arbitrary path.
line :: Path -> Picture
line    = Line

-- | A circle with the given radius.
circle  :: Float  -> Picture
circle  = Circle

-- | A circle with the given thickness and radius.
--   If the thickness is 0 then this is equivalent to `Circle`.
thickCircle  :: Float -> Float -> Picture
thickCircle = ThickCircle

-- | A circular arc drawn counter-clockwise between two angles (in degrees) 
--   at the given radius.
arc     :: Float -> Float -> Float -> Picture
arc = Arc

-- | A circular arc drawn counter-clockwise between two angles (in degrees),
--   with the given radius  and thickness.
--   If the thickness is 0 then this is equivalent to `Arc`.
thickArc :: Float -> Float -> Float -> Float -> Picture
thickArc = ThickArc

-- | Some text to draw with a vector font.
text :: String -> Picture
text = Text

-- | A bitmap image with a width, height and a Vector holding the 
--   32-bit RGBA bitmap data.
-- 
--  The boolean flag controls whether Gloss should cache the data
--  between frames for speed.
--  If you are programatically generating the image for
--  each frame then use `False`.  
--  If you have loaded it from a file then use `True`.
bitmap  :: Int -> Int -> BitmapData -> Bool -> Picture
bitmap = Bitmap

-- | A picture drawn with this color.
color :: Color -> Picture -> Picture
color = Color

-- | A picture translated by the given x and y coordinates.
translate :: Float -> Float -> Picture -> Picture
translate = Translate

-- | A picture rotated clockwise by the given angle (in degrees).
rotate  :: Float -> Picture -> Picture
rotate = Rotate

-- | A picture scaled by the given x and y factors.
scale   :: Float -> Float -> Picture -> Picture
scale = Scale

-- | A picture consisting of several others.
pictures :: [Picture] -> Picture
pictures = Pictures


-- Other Shapes ---------------------------------------------------------------
-- | A closed loop along a path.
lineLoop :: Path -> Picture
lineLoop []     = Line []
lineLoop (x:xs) = Line ((x:xs) ++ [x])


-- Circles and Arcs -----------------------------------------------------------
-- | A solid circle with the given radius.
circleSolid :: Float -> Picture
circleSolid r 
        = thickCircle (r/2) r


-- | A solid arc, drawn counter-clockwise between two angles at the given radius.
arcSolid  :: Float -> Float -> Float -> Picture
arcSolid a1 a2 r 
        = thickArc a1 a2 (r/2) r 


-- | A wireframe sector of a circle. 
--   An arc is draw counter-clockwise from the first to the second angle at
--   the given radius. Lines are drawn from the origin to the ends of the arc.
---
--   NOTE: We take the absolute value of the radius incase it's negative.
--   It would also make sense to draw the sector flipped around the 
--   origin, but I think taking the absolute value will be less surprising
--   for the user.
-- 
sectorWire :: Float -> Float -> Float -> Picture
sectorWire a1 a2 r_
 = let r        = abs r_
   in  Pictures 
        [ Arc a1 a2 r
        , Line [(0, 0), (r * cos (degToRad a1), r * sin (degToRad a1))]
        , Line [(0, 0), (r * cos (degToRad a2), r * sin (degToRad a2))] ]


-- Rectangles -----------------------------------------------------------------
-- NOTE: Only the first of these rectangle functions has haddocks on the
--       arguments to reduce the amount of noise in the extracted docs.

-- | A path representing a rectangle centered about the origin
rectanglePath 
        :: Float        -- ^ width of rectangle
        -> Float        -- ^ height of rectangle
        -> Path
rectanglePath sizeX sizeY                       
 = let  sx      = sizeX / 2
        sy      = sizeY / 2
   in   [(-sx, -sy), (-sx, sy), (sx, sy), (sx, -sy)]


-- | A wireframe rectangle centered about the origin.
rectangleWire :: Float -> Float -> Picture
rectangleWire sizeX sizeY
        = lineLoop $ rectanglePath sizeX sizeY


-- | A wireframe rectangle in the y > 0 half of the x-y plane.
rectangleUpperWire :: Float -> Float -> Picture
rectangleUpperWire sizeX sizeY
        = lineLoop $ rectangleUpperPath sizeX sizeY


-- | A path representing a rectangle in the y > 0 half of the x-y plane.
rectangleUpperPath :: Float -> Float -> Path
rectangleUpperPath sizeX sy
 = let  sx      = sizeX / 2
   in   [(-sx, 0), (-sx, sy), (sx, sy), (sx, 0)]


-- | A solid rectangle centered about the origin.
rectangleSolid :: Float -> Float -> Picture
rectangleSolid sizeX sizeY
        = Polygon $ rectanglePath sizeX sizeY


-- | A solid rectangle in the y > 0 half of the x-y plane.
rectangleUpperSolid :: Float -> Float -> Picture
rectangleUpperSolid sizeX sizeY
        = Polygon  $ rectangleUpperPath sizeX sizeY

-- | Convert a non self intersecting (possibly concave) polygon into a Picture
generalPolygon :: Path -> Picture
generalPolygon points = Pictures triangles where

    triangles :: [Picture]
    triangles = map Polygon (triangulation points)

    -- This is a super inefficient implementations
    triangulation :: [Point] -> [[Point]]
    triangulation path | length path < 3 = []
    triangulation path = case cutEar path of
        Nothing               -> [path]
        Just (ear, newPoints) -> ear : triangulation newPoints

    cutEar :: [Point] -> Maybe ([Point], [Point])
    cutEar path | n >= 3 = do -- Maybe
        (ear@[a,b,c], rest) <- find (uncurry isEar) candidateEarsAndRest
        return $ (ear, a:c:rest)
        where
            n = length path
            candidateEarsAndRest = take n . map (splitAt 3 . take n) . tails . cycle $ path
            isEar [a@(ax,ay),b@(bx,by),c@(cx,cy)] rest' = (winding * polygonWinding > 0) && (not $ any ((flip isPointInTriangle) (a,b,c)) rest')
                where
                    (a2bx, a2by) = (bx - ax, by - ay)
                    (b2cx, b2cy) = (cx - bx, cy - by)
                    winding = (b2cx * a2by) - (b2cy * a2bx)

    cutEar _ = Nothing

    {-}
    triangulation :: [Path]
    triangulation = runST $ do
        let startSize = length points
        ringRef         <- newSTRef =<< toRing points
        numToCheckRef   <- newSTRef (startSize - 2)
        sizeRef         <- newSTRef startSize
        earsRef         <- newSTRef []

        -- Keep traveling around the ring, looking for ears to cut until numToCheckRef == 0
        let
            cutEar = do
                -- Get the previous current and neighboring vertexes.
                Vertex prevV point nextV <- readSTRef ringRef
                Vertex prevPrevV prevPoint prevNextV <- readSTRef prevV
                Vertex nextPrevV nextPoint nextNextV <- readSTRef nextV

                -- update the neighboring vertices to point to eachother.
                writeSTRef (Vertex prevPrevV prevPoint nextV) prevV
                writeSTRef (Vertex prevV nextPoint nextNextV) nextV

                -- move to the previouse vertex which now needs to be rechecked.
                writeSTRef ringRef prevV
                modifySTRef (+1) numToCheckRef
                modifySTRef (-1) sizeRef

                -- Add the ear
                modifySTRef ((prevPoint, point, nextPoint):) earsRef
                return ()

            checkIsEar = do
                (ear, rest) <- currentCandidataEarAndRest
                return $ not $ any (`isPointInTriangle` ear) rest

            currentCandidataEarAndRest = do
                Vertex prevV _ _ <- readSTRef ringRef
                [a,b,c] <- take prevV 3
                _:_:_:rest <- prevV =<< readSTRef sizeRef
                return ((a,b,c), rest)

            takePoints current n = do
                Vertex _ point nextV <- readSTRef current
                rest <- takePoints nextV (n-1)
                return $ point : rest

            moveNext = do
                -- Get the previous current and neighboring vertexes.
                Vertex _ _ nextV <- readSTRef ringRef
                writeSTRef ringRef nextV

            simpleTriangulation = do
                pointsLeft <- readSTRef sizeRef
                sequence_ $ replicate (pointsLeft - 2) cutEar

            traverse = do
                numToCheck <- numToCheckRef
                if numToCheck == 0
                    then simpleTriangulation
                    else do
                        isEar <- checkIsEar
                        modifySTRef (-1) numToCheckRef
                        if isEar
                            then cutEar
                            else moveNext
                        traverse

        traverse
        readSTRef earsRef
        -}


    -- | The sign of this value indicates CW or CCW winding if positive or negative respectivelly.
    polygonWinding :: Float
    polygonWinding = sum [(ay + by) * (bx - ax) | ((ax, ay), (bx, by)) <- edges]

    edges :: [(Point, Point)]
    edges = zip points (tail cycledPoints)

    -- corners :: [(Point, Point, Point)]
    -- corners = zip3 points (drop 1 cycledPoints) (drop 2 cycledPoints)

    cycledPoints :: Path
    cycledPoints = cycle points

isPointInTriangle :: Point -> (Point, Point, Point) -> Bool
isPointInTriangle p t = let (ba,bb,bc) = toBarycentric p t in 0 < ba && ba < 1 && 0 < bb && bb < 1 && 0 < bc && bc < 1

toBarycentric :: Point -> (Point, Point, Point) -> (Float, Float, Float)
toBarycentric (x, y) ((x1,y1),(x2,y2),(x3,y3)) = (ba, bb, bc) where

    ba = (((y2-y3)*(xx3)) + ((x3-x2)*(yy3))) / denom
    bb = (((y3-y1)*(xx3)) + ((x1-x3)*(yy3))) / denom
    bc = 1 - ba - bb

    xx3 = x - x3
    yy3 = y - y3
    denom = (((y2-y3)*(x1-x3)) + ((x3-x2)*(y1-y3)))

{-}
data Ring s = Vertex
                (STRef s (Ring s))   -- ^ Previous vertex.
                Point           -- ^ Current point.
                (STRef s (Ring s))   -- ^ Next vertex.

toRing :: [Point] -> ST s (Ring s)
toRing points = mfix recMakeVertices where
    recMakeVertices vertexes = mapM newSTRef (zipWith3 Vertex rotBack points rotForward) where
        rotBack     = last vertexes : vertexes
        rotForward  = tail $ cycle vertexes
-}