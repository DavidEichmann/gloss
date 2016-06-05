{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Gloss.Internals.Rendering.Picture
        (renderPicture, generalPolygonR)
where
import Control.Monad
import Graphics.Gloss.Internals.Rendering.State
import Graphics.Gloss.Internals.Rendering.Common
import Graphics.Gloss.Internals.Rendering.Circle
import Graphics.Gloss.Internals.Rendering.Bitmap
import Graphics.Gloss.Internals.Data.Picture
import Graphics.Gloss.Internals.Data.Color
import System.Mem.StableName
import Foreign.ForeignPtr
import Data.IORef
import Data.List
import Control.Monad
import Graphics.Rendering.OpenGL                                (($=), get)
import qualified Graphics.Rendering.OpenGL.GL                   as GL
import qualified Graphics.Rendering.OpenGL.GLU.Tessellation     as GLU
import qualified Graphics.Rendering.OpenGL.GLU.Errors           as GLU
import qualified Graphics.UI.GLUT                               as GLUT
import Linear

import Debug.Trace


-- | Render a picture into the current OpenGL context.
--
--   Assumes that the OpenGL matrix mode is set to @Modelview@
--
renderPicture
        :: State        -- ^ Current rendering state.
        -> Float        -- ^ View port scale, which controls the level of detail.
                        --   Use 1.0 to start with.
        -> Picture      -- ^ Picture to render.
        -> IO ()

renderPicture state circScale picture
 = do
        -- Setup render state for world
        setLineSmooth   (stateLineSmooth state)
        setBlendAlpha   (stateBlendAlpha state)
        
        -- Draw the picture
        checkErrors "before drawPicture."
        drawPicture state circScale picture
        checkErrors "after drawPicture."


vertexRPFs :: [RPoint] -> IO ()
vertexRPFs = mapM_ (\(V2 x y) -> GL.vertex $ GL.Vertex2 (fromRational x) (fromRational y :: GL.GLfloat))

toGlMatrixF :: M33 Rational -> IO (GL.GLmatrix GL.GLfloat)
toGlMatrixF matrix = GL.newMatrix GL.RowMajor $ map fromRational ((\(V3 (V3 a b c) (V3 d e f) (V3 g h i)) ->
                                                                        [a,b,0,c
                                                                        ,d,e,0,f
                                                                        ,0,0,1,0
                                                                        ,g,h,0,i]) matrix)

drawRPicture :: State -> Rational -> RPicture -> IO ()
drawRPicture (state@State{stateModelingMatrix=modelingMatrix, stateStencils=sp}) circScale picture
  = {-# SCC "drawComponentRational" #-}
    case picture of

        RBlank  -> return ()

        -- colors with float components.
        RColor col p
         |  stateColor state
         ->  do
                let
                    RGBA r g b a  = col
                    glCol = GL.Color4 (gf r) (gf g) (gf b) (gf a)
                GL.currentColor  $= glCol
                drawRPicture state{stateCurrentColor=Just glCol} circScale p
                mapM_ (GL.currentColor $=) (stateCurrentColor state)

         |  otherwise
         ->     drawRPicture state circScale p

        RPolygon path ->
            -- drawPicture state (fromRational circScale) (Polygon $ ((map (\(V2 x y) -> (fromRational x,fromRational y)))) path)
            -- GL.renderPrimitive GL.Polygon (vertexRPFs path)
            mapM_ (drawRPicture state circScale . RPolygonConvex) (generalPolygonR path)
        
        RPolygonConvex path ->
            GL.renderPrimitive GL.Polygon (vertexRPFs path)

        RLine path ->
            GL.renderPrimitive GL.LineStrip (vertexRPFs path)

        RText str -> do
            GL.blend        $= GL.Disabled
            GL.preservingMatrix $ GLUT.renderString GLUT.Roman str
            GL.blend        $= GL.Enabled
        
        RPictures ps ->
            mapM_ (drawRPicture state circScale) ps
        
        RTransform matrix subPic -> do

            let newState@State{stateModelingMatrix=netMatrixR} = multMatrix state matrix
            netMatrixF <- toGlMatrixF netMatrixR

            GL.matrix Nothing $= netMatrixF

            let circMatrixScale = toRational . norm . fmap fromRational $ (matrix !* (V3 0 0 0)) - (matrix !* (V3 1 1 1))
            drawRPicture (multMatrix state matrix) (circScale * circMatrixScale) subPic

            modelingMatrixF <- toGlMatrixF modelingMatrix
            GL.matrix Nothing $= modelingMatrixF

        
        RStencil paths subPic -> do

            let
                pathsPic = RPictures $ map RPolygon paths

                loadStencil :: Bool -> IO ()
                loadStencil load = do
                        -- | Start editing the stencil buffer (never write into the color buffer)
                        GL.stencilFunc  $= (GL.Always, 1, 0xFF)
                        GL.stencilOp    $= (GL.OpKeep, GL.OpKeep, if load then GL.OpIncr else GL.OpDecr)
                        GL.stencilMask  $= 0xFF
                        GL.depthMask    $= GL.Disabled
                        GL.colorMask    $= GL.Color4 GL.Disabled GL.Disabled GL.Disabled GL.Disabled

                        -- | Draw the stencil path into the stencil buffer
                        drawRPicture state circScale pathsPic

                        -- | Stop editing the stencil buffer
                        GL.stencilMask  $= 0x00
                        GL.depthMask    $= GL.Enabled
                        GL.colorMask    $= GL.Color4 GL.Enabled GL.Enabled GL.Enabled GL.Enabled

            when (null sp)
              (GL.stencilTest $= GL.Enabled)

            let stencilCount = fromIntegral $ length sp

            -- | Load the stencil
            loadStencil True
            GL.stencilFunc  $= (GL.Equal, stencilCount + 1, 0xFF)

            -- | Clear the color buffer under the stencil.
            currentColor <- get GL.currentColor
            bgC          <- get GL.clearColor
            GL.currentColor  $= bgC
            drawRPicture state circScale pathsPic
            GL.currentColor  $= currentColor

            -- | Draw the picture into the stencil.
            drawRPicture state{stateStencils=paths:sp} circScale subPic

            -- | Load old stencil.
            loadStencil False
            GL.stencilFunc  $= (GL.Equal, stencilCount, 0xFF)

            -- | disable the stencil test if this is a top level stencil.
            when (null sp)
                (GL.stencilTest $= GL.Disabled)


drawPicture :: State -> Float -> Picture -> IO ()         
drawPicture (state@State{stateModelingMatrix=modelingMatrix, stateStencils=sp}) circScale picture
 = {-# SCC "drawComponent" #-}
   case picture of

        RPic rp
         -> GL.preservingMatrix $ drawRPicture state (toRational circScale) rp

        -- nothin'
        Blank
         ->     return ()

        -- line
        Line path       
         -> GL.renderPrimitive GL.LineStrip 
                $ vertexPFs state path


        -- polygon (where?)
        PolygonConvex path
         | stateWireframe state
         -> GL.renderPrimitive GL.LineLoop
                $ vertexPFs state path
                
         | otherwise
         -> GL.renderPrimitive GL.Polygon
                $ vertexPFs state path

        Polygon path
         | stateWireframe state
         -> GL.renderPrimitive GL.LineLoop
                $ vertexPFs state path
                
         | otherwise
         -> do
            triangulatedPaths <- generalPolygon path
            mapM_ (drawPicture state circScale . PolygonConvex) triangulatedPaths

        -- circle
        Circle radius
         ->  renderCircle (vertexPFs state) 0 0 circScale radius 0
        
        ThickCircle radius thickness
         ->  renderCircle (vertexPFs state) 0 0 circScale radius thickness
        
        -- arc
        Arc a1 a2 radius
         ->  renderArc (vertexPFs state) 0 0 circScale radius a1 a2 0
             
        ThickArc a1 a2 radius thickness
         ->  renderArc (vertexPFs state) 0 0 circScale radius a1 a2 thickness
             
        -- stroke text
        --      text looks weird when we've got blend on,
        --      so disable it during the renderString call.
        Text str 
         -> do
                GL.blend        $= GL.Disabled
                GL.preservingMatrix $ GLUT.renderString GLUT.Roman str
                GL.blend        $= GL.Enabled
             
        -- Stencil -------------------------------
        Stencil paths pic
         | null paths
         -> return ()
         
         | otherwise
         -> do
                -- | If smaller than a pixel, then don't render
                let
                    allPoints = concat paths
                    minX = minimum . map fst $ allPoints
                    maxX = maximum . map fst $ allPoints
                    minY = minimum . map snd $ allPoints
                    maxY = maximum . map snd $ allPoints
                    maxExtent = circScale * (max (maxX - minX) (maxY - minY))

                    pathsPic = Pictures $ map Polygon $ filter ((>2) . length . take 3) paths

                    loadStencil :: Bool -> IO ()
                    loadStencil load = do
                            -- | Start editing the stencil buffer (never write into the color buffer)
                            GL.stencilFunc  $= (GL.Always, 1, 0xFF)
                            GL.stencilOp    $= (GL.OpKeep, GL.OpKeep, if load then GL.OpIncr else GL.OpDecr)
                            GL.stencilMask  $= 0xFF
                            GL.depthMask    $= GL.Disabled
                            GL.colorMask    $= GL.Color4 GL.Disabled GL.Disabled GL.Disabled GL.Disabled

                            -- | Draw the stencil path into the stencil buffer
                            drawPicture state circScale pathsPic

                            -- | Stop editing the stencil buffer
                            GL.stencilMask  $= 0x00
                            GL.depthMask    $= GL.Enabled
                            GL.colorMask    $= GL.Color4 GL.Enabled GL.Enabled GL.Enabled GL.Enabled

                -- | TODO also check if the stencil is out of the screen.
                -- | TODO have a maximum depth?

                if False -- maxExtent < 1
                    then return ()
                    else do
                        when (null sp)
                          (GL.stencilTest $= GL.Enabled)

                        let stencilCount = fromIntegral $ length sp

                        -- | Load the stencil
                        loadStencil True
                        GL.stencilFunc  $= (GL.Equal, stencilCount + 1, 0xFF)

                        -- | Clear the color buffer under the stencil.
                        currentColor <- get GL.currentColor
                        bgC          <- get GL.clearColor
                        GL.currentColor  $= bgC
                        drawPicture state circScale pathsPic
                        GL.currentColor  $= currentColor

                        -- | Draw the picture into the stencil.
                        let pathsR = (map . map) (\(x,y) -> fmap toRational (V2 x y)) paths
                        drawPicture state{stateStencils=pathsR:sp} circScale pic

                        -- | Load old stencil.
                        loadStencil False
                        GL.stencilFunc  $= (GL.Equal, stencilCount, 0xFF)

                        -- | disable the stencil test if this is a top level stencil.
                        when (null sp)
                            (GL.stencilTest $= GL.Disabled)

        -- colors with float components.
        Color col p
         |  stateColor state
         ->  do oldColor         <- get GL.currentColor

                let RGBA r g b a  = col

                GL.currentColor  $= GL.Color4 (gf r) (gf g) (gf b) (gf a)
                drawPicture state circScale p
                GL.currentColor  $= oldColor

         |  otherwise
         ->     drawPicture state circScale p


        -- Translation --------------------------
        -- Easy translations are done directly to avoid calling GL.perserveMatrix.
        -- Translate posX posY (Circle radius)
        --  -> renderCircle posX posY circScale radius 0

        -- Translate posX posY (ThickCircle radius thickness)
        --  -> renderCircle posX posY circScale radius thickness

        -- Translate posX posY (Arc a1 a2 radius)
        --  -> renderArc posX posY circScale radius a1 a2 0

        -- Translate posX posY (ThickArc a1 a2 radius thickness)
        --  -> renderArc posX posY circScale radius a1 a2 thickness
             
        -- Translate tx ty (Rotate deg p)
        --  -> GL.preservingMatrix
        --   $ do  GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
        --         GL.rotate    (gf deg) (GL.Vector3 0 0 (-1))
        --         drawPicture state circScale p

        Translate tx ty p
         -- -> GL.preservingMatrix
         --  $ do  GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
         --        drawPicture state circScale p
         -> drawPicture (multMatrix state newMatrix) circScale p
          where
            newMatrix = (fmap.fmap) toRational $ V3
              (V3 1 0 tx)
              (V3 0 1 ty)
              (V3 0 0 1)


        -- Rotation -----------------------------
        -- Easy rotations are done directly to avoid calling GL.perserveMatrix.
        -- Rotate _   (Circle radius)
        --  -> renderCircle   0 0 circScale radius 0

        -- Rotate _   (ThickCircle radius thickness)
        --  -> renderCircle   0 0 circScale radius thickness

        -- Rotate deg (Arc a1 a2 radius)
        --  -> renderArc      0 0 circScale radius (a1-deg) (a2-deg) 0

        -- Rotate deg (ThickArc a1 a2 radius thickness)
        --  -> renderArc      0 0 circScale radius (a1-deg) (a2-deg) thickness

        
        Rotate deg p
         -> drawPicture (multMatrix state newMatrix) circScale p
          where
            ccwRad = negate (deg * pi / 180)
            s = sin ccwRad
            c = cos ccwRad
            newMatrix = (fmap.fmap) toRational $ V3
              (V3 c (-s) 0)
              (V3 s   c  0)
              (V3 0   0  1)


        -- Scale --------------------------------
        Scale sx sy p
         -> drawPicture (multMatrix state newMatrix) circScale p
          where
            newMatrix = (fmap.fmap) toRational $ V3
              (V3 sx  0  0)
              (V3 0  sy  0)
              (V3 0   0  1)

        -- Transform --------------------------------
        Transform newMatrixF p
         -> drawPicture (multMatrix state newMatrix) circScale p
          where
            newMatrix = (fmap.fmap) toRational $ newMatrixF

        -- Bitmap -------------------------------
        Bitmap width height imgData cacheMe
         -> do  
                let rowInfo =
                      case rowOrder (bitmapFormat imgData) of
                         BottomToTop -> [(0,0), (1,0), (1,1), (0,1)]
                         TopToBottom -> [(0,1), (1,1), (1,0), (0,0)]

                -- Load the image data into a texture,
                -- or grab it from the cache if we've already done that before.
                tex     <- loadTexture (stateTextures state) width height imgData cacheMe
         
                -- Set up wrap and filtering mode
                GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
                GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
                GL.textureFilter   GL.Texture2D      $= ((GL.Nearest, Nothing), GL.Nearest)
                
                -- Enable texturing
                GL.texture GL.Texture2D $= GL.Enabled
                GL.textureFunction      $= GL.Combine
                
                -- Set current texture
                GL.textureBinding GL.Texture2D $= Just (texObject tex)
                
                -- Set to opaque
                oldColor <- get GL.currentColor
                GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0
                
                -- Draw textured polygon
                GL.renderPrimitive GL.Polygon
                 $ zipWithM_
                        (\(pX, pY) (tX, tY)
                          -> do GL.texCoord $ GL.TexCoord2 (gf tX) (gf tY)
                                vertexPFs state [(pX, pY)])

                        (bitmapPath (fromIntegral width) (fromIntegral height))
                                rowInfo

                -- Restore color
                GL.currentColor $= oldColor

                -- Disable texturing
                GL.texture GL.Texture2D $= GL.Disabled

                -- Free uncachable texture objects.
                freeTexture tex
                

        Pictures ps
         -> mapM_ (drawPicture state circScale) ps
        


-- Errors ---------------------------------------------------------------------
checkErrors :: String -> IO ()
checkErrors place
 = do   errors          <- get $ GLU.errors
        when (not $ null errors)
         $ mapM_ (handleError place) errors

handleError :: String -> GLU.Error -> IO ()
handleError place err
 = case err of
    GLU.Error GLU.StackOverflow _
     -> error $ unlines 
      [ "Gloss / OpenGL Stack Overflow " ++ show place
      , "  This program uses the Gloss vector graphics library, which tried to"
      , "  draw a picture using more nested transforms (Translate/Rotate/Scale)"
      , "  than your OpenGL implementation supports. The OpenGL spec requires"
      , "  all implementations to have a transform stack depth of at least 32,"
      , "  and Gloss tries not to push the stack when it doesn't have to, but"
      , "  that still wasn't enough."
      , ""
      , "  You should complain to your harware vendor that they don't provide"
      , "  a better way to handle this situation at the OpenGL API level."
      , ""
      , "  To make this program work you'll need to reduce the number of nested"
      , "  transforms used when defining the Picture given to Gloss. Sorry." ]

    -- Issue #32: Spurious "Invalid Operation" errors under Windows 7 64-bit.
    --   When using GLUT under Windows 7 it complains about InvalidOperation, 
    --   but doesn't provide any other details. All the examples look ok, so 
    --   we're just ignoring the error for now.
    GLU.Error GLU.InvalidOperation _
     -> return ()
    _ 
     -> error $ unlines 
     [  "Gloss / OpenGL Internal Error " ++ show place
     ,  "  Please report this on haskell-gloss@googlegroups.com."
     ,  show err ]


-- Textures -------------------------------------------------------------------
-- | Load a texture.
--   If we've seen it before then use the pre-installed one from the texture
--   cache, otherwise load it into OpenGL.
loadTexture
        :: IORef [Texture]
        -> Int -> Int -> BitmapData
        -> Bool
        -> IO Texture

loadTexture refTextures width height imgData cacheMe
 = do   textures        <- readIORef refTextures

        -- Try and find this same texture in the cache.
        name            <- makeStableName imgData
        let mTexCached      
                = find (\tex -> texName   tex == name
                             && texWidth  tex == width
                             && texHeight tex == height)
                textures
                
        case mTexCached of
         Just tex
          ->    return tex
                
         Nothing
          -> do tex     <- installTexture width height imgData cacheMe
                when cacheMe
                 $ writeIORef refTextures (tex : textures)
                return tex


-- | Install a texture into OpenGL.
installTexture     
        :: Int -> Int
        -> BitmapData
        -> Bool
        -> IO Texture

installTexture width height bitmapData@(BitmapData _ fmt fptr) cacheMe
 = do   
        let glFormat = case pixelFormat fmt of
                           PxABGR -> GL.RGBA
                           PxRGBA -> GL.ABGR
        -- Allocate texture handle for texture
        [tex] <- GL.genObjectNames 1
        GL.textureBinding GL.Texture2D $= Just tex

        -- Sets the texture in imgData as the current texture
        -- This copies the data from the pointer into OpenGL texture memory, 
        -- so it's ok if the foreignptr gets garbage collected after this.
        withForeignPtr fptr
         $ \ptr ->
           GL.texImage2D
                GL.Texture2D
                GL.NoProxy
                0
                GL.RGBA8
                (GL.TextureSize2D
                        (gsizei width)
                        (gsizei height))
                0
                (GL.PixelData glFormat GL.UnsignedInt8888 ptr)

        -- Make a stable name that we can use to identify this data again.
        -- If the user gives us the same texture data at the same size then we
        -- can avoid loading it into texture memory again.
        name    <- makeStableName bitmapData

        return  Texture
                { texName       = name
                , texWidth      = width
                , texHeight     = height
                , texData       = fptr
                , texObject     = tex
                , texCacheMe    = cacheMe }


-- | If this texture does not have its `cacheMe` flag set then delete it from 
--   OpenGL and free the GPU memory.
freeTexture :: Texture -> IO ()
freeTexture tex
 | texCacheMe tex       = return ()
 | otherwise            = GL.deleteObjectNames [texObject tex]



-- Utils ----------------------------------------------------------------------
-- | Turn alpha blending on or off
setBlendAlpha :: Bool -> IO ()
setBlendAlpha state
        | state 
        = do    GL.blend        $= GL.Enabled
                GL.blendFunc    $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

        | otherwise
        = do    GL.blend        $= GL.Disabled
                GL.blendFunc    $= (GL.One, GL.Zero)    

-- | Turn line smoothing on or off
setLineSmooth :: Bool -> IO ()
setLineSmooth state
        | state         = GL.lineSmooth $= GL.Enabled
        | otherwise     = GL.lineSmooth $= GL.Disabled


vertexPFs :: State -> [(Float, Float)] -> IO ()
vertexPFs _ [] = return ()
vertexPFs State{stateModelingMatrix=mR} ps
    = mapM_ (\(V3 x y w) -> GL.vertex $ GL.Vertex2 (gf (x / w)) (gf (y / w))) (map (\(x, y) -> m !* (V3 x y 1)) ps)
    where
        m = (fmap . fmap) fromRational mR
{-# INLINE vertexPFs #-}





-- | Convert a non self intersecting (possibly concave) polygon into a Picture
generalPolygon :: Path -> IO [Path]
generalPolygon points = {-# SCC "triangulatePolygon" #-} triangulation points where

    triangulationToPaths :: GLU.Triangulation () -> [Path]
    triangulationToPaths (GLU.Triangulation triangles) = map (\(GLU.Triangle
                                                                (GLU.AnnotatedVertex (GL.Vertex3 x1 y1 _) _)
                                                                (GLU.AnnotatedVertex (GL.Vertex3 x2 y2 _) _)
                                                                (GLU.AnnotatedVertex (GL.Vertex3 x3 y3 _) _))
                                                                -> [(realToFrac x1, realToFrac y1), (realToFrac x2, realToFrac y2), (realToFrac x3, realToFrac y3)]) triangles

    triangulation' :: [Point] -> IO [Path]
    triangulation' ps = do
        glutTri <- GLU.triangulate              -- Tessellator Triangulation Point = TessWinding -> Tolerance -> Normal3 GLdouble -> Combiner Point -> ComplexPolygon Point -> IO (Triangulation Point)
                            GLU.TessWindingOdd
                            0
                            (GL.Normal3 0 0 1)
                            (\_ _ -> ())
                            (GLU.ComplexPolygon [GLU.ComplexContour (zipWith GLU.AnnotatedVertex (map (\(x, y) -> GL.Vertex3 (realToFrac x) (realToFrac y) 0) ps) (repeat ()))])

        return $ triangulationToPaths glutTri


    -- This is a super inefficient implementation
    triangulation :: [Point] -> IO [Path]
    triangulation = return . triangulation''

    triangulation'' :: [Point] -> [Path]
    triangulation'' path | length path < 3 = []
    triangulation'' path = case cutEar path of
        Nothing               -> [path]
        Just (ear, newPoints) -> ear : triangulation'' newPoints

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


    -- | The sign of this value indicates CW or CCW winding if positive or negative respectivelly.
    polygonWinding :: Float
    polygonWinding = sum [(ay + by) * (bx - ax) | ((ax, ay), (bx, by)) <- edges]

    edges :: [(Point, Point)]
    edges = zip points (tail cycledPoints)

    -- corners :: [(Point, Point, Point)]
    -- corners = zip3 points (drop 1 cycledPoints) (drop 2 cycledPoints)

    cycledPoints :: Path
    cycledPoints = cycle points

-- | Convert a non self intersecting (possibly concave) polygon into a Picture
generalPolygonR :: RPath -> [RPath]
generalPolygonR points = {-# SCC "triangulatePolygonR" #-} triangulation'' points where

    -- This is a super inefficient implementation
    triangulation'' :: [RPoint] -> [RPath]
    triangulation'' path | length path < 3 = []
    triangulation'' path = case cutEar path of
        Nothing               -> [path]
        Just (ear, newPoints) -> ear : triangulation'' newPoints

    cutEar :: [RPoint] -> Maybe ([RPoint], [RPoint])
    cutEar path | n >= 3 = do -- Maybe
        (ear@[a,b,c], rest) <- find (uncurry isEar) candidateEarsAndRest
        return $ (ear, a:c:rest)
        where
            n = length path
            candidateEarsAndRest = take n . map (splitAt 3 . take n) . tails . cycle $ path

            isEar :: [RPoint] -> [RPoint] -> Bool
            isEar [a,b,c] rest' = (winding * polygonWinding > 0) && (not $ any ((flip isPointInTriangleR) (a,b,c)) rest')
                where
                    (V2 a2bx a2by) = b - a
                    (V2 b2cx b2cy) = c - b
                    winding = (b2cx * a2by) - (b2cy * a2bx)

    cutEar _ = Nothing


    -- | The sign of this value indicates CW or CCW winding if positive or negative respectivelly.
    polygonWinding :: Rational
    polygonWinding = sum [(ay + by) * (bx - ax) | ((V2 ax ay), (V2 bx by)) <- edges]

    edges :: [(RPoint, RPoint)]
    edges = zip points (tail cycledPoints)

    -- corners :: [(Point, Point, Point)]
    -- corners = zip3 points (drop 1 cycledPoints) (drop 2 cycledPoints)

    cycledPoints :: RPath
    cycledPoints = cycle points

data Winding = CCW | CW

isPointInConvexPoly :: (Fractional a, Eq a) => Winding -> [V2 a] -> V2 a -> Bool
isPointInConvexPoly winding path p = all isPointOnCorrectSideOfEdge edges
    where
        cleanPath = map head . group $ path
        edges = let cleanPathA:cleanPathB:_ = permutations cleanPath in zip cleanPathA cleanPathB
        isOnCorrectSideSignum = case winding of
            CCW -> 1
            CW  -> (-1)
        isPointOnCorrectSideOfEdge (a, b) = isOnCorrectSideSignum == signum ((b - a) `crossZ` (p - b))

isPointInTriangleR :: (Fractional a, Ord a) => V2 a -> (V2 a, V2 a, V2 a) -> Bool
isPointInTriangleR p (a,b,c) = isPointInConvexPoly CCW [a,b,c] p
--isPointInTriangleR p t = let (ba,bb,bc) = toBarycentricR p t in 0 < ba && ba < 1 && 0 < bb && bb < 1 && 0 < bc && bc < 1

toBarycentricR :: Fractional a => V2 a -> (V2 a, V2 a, V2 a) -> (a, a, a)
toBarycentricR (V2 x y) ((V2 x1 y1), (V2 x2 y2), (V2 x3 y3)) = (ba, bb, bc) where

    ba = (((y2-y3)*(xx3)) + ((x3-x2)*(yy3))) / denom
    bb = (((y3-y1)*(xx3)) + ((x1-x3)*(yy3))) / denom
    bc = 1 - ba - bb

    xx3 = x - x3
    yy3 = y - y3
    denom = (((y2-y3)*(x1-x3)) + ((x3-x2)*(y1-y3)))

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