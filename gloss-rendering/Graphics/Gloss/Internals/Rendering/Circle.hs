{-# OPTIONS_HADDOCK hide #-}

-- | Fast(ish) rendering of circles.
module Graphics.Gloss.Internals.Rendering.Circle
        ( renderCircle
        , renderArc)
where
import  Graphics.Gloss.Internals.Rendering.Common
import  GHC.Exts
import  qualified Graphics.Rendering.OpenGL.GL          as GL


-- | Decide how many line segments to use to render the circle.
--   The number of segments we should use to get a nice picture depends on 
--   the size of the circle on the screen, not its intrinsic radius.
--   If the viewport has been zoomed-in then we need to use more segments.
--
circleSteps :: Float -> Int
circleSteps sDiam
        | sDiam < 8     = 8
        | sDiam < 16    = 16
        | sDiam < 32    = 32
        | otherwise     = 64
{-# INLINE circleSteps #-}


-- Circle ---------------------------------------------------------------------
-- | Render a circle with the given thickness
renderCircle :: ([(Float, Float)] -> IO ()) -> Float -> Float -> Float -> Float -> Float -> IO ()
renderCircle drawVertexPFs posX posY scaleFactor radius_ thickness_
 = go (abs radius_) (abs thickness_)
 where go radius thickness

        -- If the circle is smaller than a pixel, render it as a point.
        | thickness     == 0
        , radScreen     <- scaleFactor * (radius / 2)
        , radScreen     <= 1
        = GL.renderPrimitive GL.Points $ drawVertexPFs [(posX, posY)]

        -- Render zero thickness circles with lines.
        | thickness == 0
        , radScreen     <- scaleFactor * radius
        , steps         <- circleSteps radScreen
        = renderCircleLine drawVertexPFs posX posY steps radius

        -- Some thick circle.
        | radScreen     <- scaleFactor * (radius + thickness / 2)
        , steps         <- circleSteps radScreen
        = renderCircleStrip drawVertexPFs posX posY steps radius thickness


-- | Render a circle as a line.
renderCircleLine :: ([(Float, Float)] -> IO ()) -> Float -> Float -> Int -> Float -> IO ()
renderCircleLine drawVertexPFs (posX) (posY) steps (rad)
 = let  n               = fromIntegral steps
        !(tStep)     = (2 * pi) / n
        !(tStop)     = (2 * pi)

   in   GL.renderPrimitive GL.LineLoop
         $ renderCircleLine_step drawVertexPFs posX posY tStep tStop rad 0.0
{-# INLINE renderCircleLine #-}


-- | Render a circle with a given thickness as a triangle strip
renderCircleStrip :: ([(Float, Float)] -> IO ()) -> Float -> Float -> Int -> Float -> Float -> IO ()
renderCircleStrip drawVertexPFs (posX) (posY) steps r width
 = let  n               = fromIntegral steps
        !(tStep)     = (2 * pi) / n
        !(tStop)     = (2 * pi) + (tStep) / 2
        !(r1)        = r - width / 2
        !(r2)        = r + width / 2

   in   GL.renderPrimitive GL.TriangleStrip
         $ renderCircleStrip_step drawVertexPFs posX posY tStep tStop r1 0.0 r2 
                (tStep / 2.0)
{-# INLINE renderCircleStrip #-}


-- Arc ------------------------------------------------------------------------
-- | Render an arc with the given thickness.
renderArc :: ([(Float, Float)] -> IO ()) -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> IO ()
renderArc drawVertexPFs posX posY scaleFactor radius_ a1 a2 thickness_
 = go (abs radius_) (abs thickness_)
 where go radius thickness

        -- Render zero thickness arcs with lines.
        | thickness == 0
        , radScreen     <- scaleFactor * radius
        , steps         <- circleSteps radScreen
        = renderArcLine drawVertexPFs posX posY steps radius a1 a2

        -- Some thick arc.
        | radScreen     <- scaleFactor * (radius + thickness / 2)
        , steps         <- circleSteps radScreen
        = renderArcStrip drawVertexPFs posX posY steps radius a1 a2 thickness
  

-- | Render an arc as a line.
renderArcLine :: ([(Float, Float)] -> IO ()) -> Float -> Float -> Int -> Float -> Float -> Float -> IO ()
renderArcLine drawVertexPFs (posX) (posY) steps (rad) a1 a2
 = let  n               = fromIntegral steps
        !(tStep)     = (2 * pi) / n
        !(tStart)    = degToRad a1
        !(tStop)     = degToRad a2 + if a1 >= a2 then 2 * pi else 0

        -- force the line to end at the desired angle
        endVertex       = addPointOnCircle drawVertexPFs posX posY rad tStop

   in   GL.renderPrimitive GL.LineStrip
         $ do   renderCircleLine_step drawVertexPFs posX posY tStep tStop rad tStart
                endVertex
{-# INLINE renderArcLine #-}


-- | Render an arc with a given thickness as a triangle strip
renderArcStrip :: ([(Float, Float)] -> IO ()) -> Float -> Float -> Int -> Float -> Float -> Float -> Float -> IO ()
renderArcStrip drawVertexPFs (posX) (posY) steps r a1 a2 width
 = let  n               = fromIntegral steps
        tStep           = (2 * pi) / n

        t1              = normalizeAngle $ degToRad a1
        t2              = normalizeAngle $ degToRad a2
        (tStart, tStop) = if t1 <= t2 then (t1, t2) else (t2, t1)
        tDiff           = tStop - tStart
        tMid            = tStart + tDiff / 2

        !(tStep')    = tStep
        !(tStep2')   = tStep / 2
        !(tStart')   = tStart
        !(tStop')    = tStop
        !(tCut')     = tStop - tStep
        !(tMid')     = tMid
        !(r1')       = r - width / 2
        !(r2')       = r + width / 2
                
   in   GL.renderPrimitive GL.TriangleStrip
         $ do  
                 -- start vector
                 addPointOnCircle drawVertexPFs posX posY r1' tStart'
                 addPointOnCircle drawVertexPFs posX posY r2' tStart'

                 -- If we don't have a complete step then just drop a point
                 -- between the two ending lines.
                 if tDiff < tStep
                   then do
                        addPointOnCircle drawVertexPFs posX posY r1' tMid'

                        -- end vectors
                        addPointOnCircle drawVertexPFs posX posY r2' tStop'
                        addPointOnCircle drawVertexPFs posX posY r1' tStop'


                   else do
                        renderCircleStrip_step drawVertexPFs posX posY tStep' tCut' r1' tStart' r2'
                                (tStart' + tStep2')

                        -- end vectors
                        addPointOnCircle drawVertexPFs posX posY r1' tStop'
                        addPointOnCircle drawVertexPFs posX posY r2' tStop'
{-# INLINE renderArcStrip #-}


-- Step functions -------------------------------------------------------------
renderCircleLine_step
        :: ([(Float, Float)] -> IO ())
        -> Float -> Float
        -> Float -> Float
        -> Float -> Float 
        -> IO ()

renderCircleLine_step drawVertexPFs posX posY tStep tStop rad tt
        | tt >= tStop
        = return ()
        
        | otherwise
        = do    addPointOnCircle drawVertexPFs posX posY rad tt
                renderCircleLine_step drawVertexPFs posX posY tStep tStop rad 
                        (tt + tStep)
{-# INLINE renderCircleLine_step #-}


renderCircleStrip_step 
        :: ([(Float, Float)] -> IO ())
        -> Float -> Float 
        -> Float -> Float 
        -> Float -> Float
        -> Float -> Float -> IO ()

renderCircleStrip_step drawVertexPFs posX posY tStep tStop r1 t1 r2 t2
        | t1 >= tStop
        = return ()
        
        | otherwise
        = do    addPointOnCircle drawVertexPFs posX posY r1 t1
                addPointOnCircle drawVertexPFs posX posY r2 t2
                renderCircleStrip_step drawVertexPFs posX posY tStep tStop r1 
                        (t1 + tStep) r2 (t2 + tStep)
{-# INLINE renderCircleStrip_step #-}


-- addPoint :: Float -> Float -> IO ()
-- addPoint x y =
--   GL.vertex $ GL.Vertex2 (gf (x)) (gf (y))
-- {-# INLINE addPoint #-}


addPointOnCircle :: ([(Float, Float)] -> IO ()) -> Float -> Float -> Float -> Float -> IO ()
addPointOnCircle drawVertexPFs posX posY rad tt =
  drawVertexPFs [(
        posX + (rad * (cos tt)),
        posY + (rad * (sin tt))
    )]
{-# INLINE addPointOnCircle #-}


-- | Convert degrees to radians
{-# INLINE degToRad #-}
degToRad :: Float -> Float
degToRad d      = d * pi / 180


-- | Normalise an angle to be between 0 and 2*pi radians
{-# INLINE normalizeAngle #-}
normalizeAngle :: Float -> Float
normalizeAngle f = f - 2 * pi * floor' (f / (2 * pi))
 where  floor' :: Float -> Float
        floor' x = fromIntegral (floor x :: Int)


{- Unused sector drawing code.
   Sectors are currently drawn as compound Pictures,
   but we might want this if we end up implementing the ThickSector 
   version as well.

-- | Render a sector as a line.
renderSectorLine :: Float -> Float -> Int -> Float -> Float -> Float -> IO ()
renderSectorLine pX@(posX) pY@(posY) steps (rad) a1 a2
 = let  n               = fromIntegral steps
        !(tStep)     = (2 * pi) / n
        !(tStart)    = degToRad a1
        !(tStop)     = degToRad a2 + if a1 >= a2 then 2 * pi else 0

        -- need to set up the edges of the start/end triangles
        startVertex     = GL.vertex $ GL.Vertex2 (gf pX) (gf pY)
        endVertex       = addPointOnCircle posX posY rad tStop

   in   GL.renderPrimitive GL.LineLoop
         $ do   startVertex
                renderCircleLine_step posX posY tStep tStop rad tStart
                endVertex

-- | Render a sector.
renderSector :: Float -> Float -> Float -> Float -> Float -> Float -> IO ()
renderSector posX posY scaleFactor radius a1 a2
        | radScreen     <- scaleFactor * radius
        , steps         <- circleSteps (2 * radScreen)
        = renderSectorLine posX posY steps radius a1 a2
-}

