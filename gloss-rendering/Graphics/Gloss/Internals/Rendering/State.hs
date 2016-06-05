{-# OPTIONS_HADDOCK hide #-}

-- | Rendering options
module Graphics.Gloss.Internals.Rendering.State
        ( State (..)
        , initState
        , Texture (..)
        , multMatrix)
where
import Graphics.Gloss.Internals.Data.Picture
import Foreign.ForeignPtr
import System.Mem.StableName
import Data.Word
import Data.IORef
import qualified Graphics.Rendering.OpenGL.GL   as GL
import Linear


-- | Abstract Gloss render state which holds references to textures
--   loaded into the GPU context.
data State
        = State
        { -- | Whether to use color
          stateColor            :: !Bool
        , stateCurrentColor     :: !(Maybe (GL.Color4 GL.GLfloat))

        -- | Whether to force wireframe mode only
        , stateWireframe        :: !Bool

        -- | Whether to use alpha blending
        , stateBlendAlpha       :: !Bool

        -- | Whether to use line smoothing
        , stateLineSmooth       :: !Bool
        
        -- | Cache of Textures that we've sent to OpenGL.
        , stateTextures         :: !(IORef [Texture])

        -- | Active modeling matrix
        , stateModelingMatrix   :: M33 RType

        -- | Current stencil in current modeling space.
        , stateStencils :: [[RPath]]
        }
        

-- | A texture that we've sent to OpenGL.
data Texture
        = Texture
        { -- | Stable name derived from the `BitmapData` that the user gives us.
          texName       :: StableName BitmapData

        -- | Width of the image, in pixels.
        , texWidth      :: Int

        -- | Height of the image, in pixels.
        , texHeight     :: Int

        -- | Pointer to the Raw texture data.
        , texData       :: ForeignPtr Word8
        
        -- | The OpenGL texture object.
        , texObject     :: GL.TextureObject

        -- | Whether we want to leave this in OpenGL texture memory between frames.
        , texCacheMe    :: Bool

         }

multMatrix :: State -> M33 RType -> State
multMatrix state@State{stateModelingMatrix=mm,stateStencils=sp} newMatrix = state{
        stateModelingMatrix = mm !*! newMatrix,
        stateStencils     = (map . map) (map $ toV2 . (toNewSpace !*) . toV3) sp
    }
    where
        toV2 (V3 x y w) = V2 (x/w) (y/w)
        toV3 (V2 x y) = V3 x y 1

        toNewSpace :: M33 RType
        toNewSpace = inv33 newMatrix

-- | A mutable render state holds references to the textures currently loaded
--   into the OpenGL context. To ensure that textures are cached in GPU memory,
--   pass the same `State` each time you call `displayPicture` or `renderPicture`.
initState :: IO State
initState
 = do   textures        <- newIORef []
        return  State
                { stateColor            = True
                , stateCurrentColor     = Just (GL.Color4 0 0 0 1)
                , stateWireframe        = False
                , stateBlendAlpha       = True
                , stateLineSmooth       = False 
                , stateTextures         = textures
                , stateModelingMatrix   = identity
                , stateStencils         = [] }
        
