{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS -fno-warn-orphans #-}

-- | Data types for representing pictures.
module Graphics.Gloss.Internals.Data.Picture
        ( Point
        , Vector
        , Path
        , Picture(..)

        , RType
        , RPoint
        , RPath
        , RMatrix
        , RPicture(..)

        -- * Bitmaps
        , BitmapData, PixelFormat(..), BitmapFormat(..), RowOrder(..)
        , bitmapOfForeignPtr
        , bitmapOfByteString
        , bitmapOfBMP
        , loadBMP)

where
import Graphics.Gloss.Internals.Data.Color
import Graphics.Gloss.Internals.Rendering.Bitmap
import Codec.BMP
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Data.Word
import Data.Monoid
import Data.ByteString
import Data.Data
import System.IO.Unsafe
import qualified Data.ByteString.Unsafe as BSU
import Prelude hiding (map)
import Linear


-- | A point on the x-y plane.
type Point      = (Float, Float)                        


-- | Pretend a point is a number.
--      Vectors aren't real numbers according to Haskell, because they don't
--      support the multiply and divide field operators. We can pretend they
--      are though, and use the (+) and (-) operators as component-wise
--      addition and subtraction.
--
instance Num Point where
        (+) (x1, y1) (x2, y2)   = (x1 + x2, y1 + y2)
        (-) (x1, y1) (x2, y2)   = (x1 - x2, y1 - y2)
        (*) (x1, y1) (x2, y2)   = (x1 * x2, y1 * y2)
        signum (x, y)           = (signum x, signum y)
        abs    (x, y)           = (abs x, abs y)
        negate (x, y)           = (negate x, negate y)  
        fromInteger x           = (fromInteger x, fromInteger x)


-- | A vector can be treated as a point, and vis-versa.
type Vector     = Point


-- | A path through the x-y plane.
type Path       = [Point]                               


-- | Better pictures
type RType = Rational
type RPoint = V2 RType
type RPath  = [RPoint]
type RMatrix = M33 RType
data RPicture
        = RBlank
        | RColor Color RPicture
        | RPolygon RPath
        | RPolygonConvex RPath
        | RLine RPath
        | RText String
        | RStencil [RPath] RPicture
        | RPictures [RPicture]
        | RTransform RMatrix RPicture
        deriving (Show, Eq, Data, Typeable)

-- | A 2D picture
data Picture
        -- Primitives -------------------------------------

        -- | A blank picture, with nothing in it.
        = RPic RPicture

        | Blank

        -- | A polygon filled with a solid color.
        | Polygon       Path

        -- | A convex polygon filled with a solid color.
        | PolygonConvex Path
        
        -- | A line along an arbitrary path.
        | Line          Path

        -- | A circle with the given radius.
        | Circle        Float

        -- | A circle with the given thickness and radius.
        --   If the thickness is 0 then this is equivalent to `Circle`.
        | ThickCircle   Float Float

        -- | A circular arc drawn counter-clockwise between two angles 
        --  (in degrees) at the given radius.
        | Arc           Float Float Float

        -- | A circular arc drawn counter-clockwise between two angles 
        --  (in degrees), with the given radius  and thickness.
        --   If the thickness is 0 then this is equivalent to `Arc`.
        | ThickArc      Float Float Float Float

        -- | Some text to draw with a vector font.
        | Text          String

        -- | A stenciled picture
        | Stencil       [Path]    Picture

        -- | A bitmap image with a width, height and some 32-bit RGBA
        --   bitmap data.
        --
        --  The boolean flag controls whether Gloss should cache the data
        --  in GPU memory between frames. If you are programatically generating
        --  the image for each frame then use @False@. If you have loaded it
        --  from a file then use @True@. 
        --  Setting @False@ for static images will make rendering slower
        --  than it needs to be.
        --  Setting @True@  for dynamically generated images will cause a
        --  GPU memory leak.
        | Bitmap        Int     Int     BitmapData Bool

        -- Color ------------------------------------------
        -- | A picture drawn with this color.
        | Color         Color           Picture

        -- Transforms -------------------------------------
        -- | A picture translated by the given x and y coordinates.
        | Translate     Float Float     Picture

        -- | A picture rotated clockwise by the given angle (in degrees).
        | Rotate        Float           Picture

        -- | A picture scaled by the given x and y factors.
        | Scale         Float   Float   Picture

        -- | Row major transformation matrix.
        | Transform     (M33 Float)   Picture

        -- More Pictures ----------------------------------
        -- | A picture consisting of several others.
        | Pictures      [Picture]
        deriving (Show, Eq, Data, Typeable)


-- Instances ------------------------------------------------------------------
instance Monoid Picture where
        mempty          = Blank
        mappend a b     = Pictures [a, b]
        mconcat         = Pictures


-- Bitmaps --------------------------------------------------------------------
-- | O(1). Use a `ForeignPtr` of RGBA data as a bitmap with the given
--   width and height.
--
--   The boolean flag controls whether Gloss should cache the data
--   between frames for speed. If you are programatically generating
--   the image for each frame then use `False`. If you have loaded it
--   from a file then use `True`.
bitmapOfForeignPtr :: Int -> Int -> BitmapFormat -> ForeignPtr Word8 -> Bool -> Picture
bitmapOfForeignPtr width height fmt fptr cacheMe
 = let  len     = width * height * 4
        bdata   = BitmapData len fmt fptr
   in   Bitmap width height bdata cacheMe


-- | O(size). Copy a `ByteString` of RGBA data into a bitmap with the given
--   width and height.
--
--   The boolean flag controls whether Gloss should cache the data
--   between frames for speed. If you are programatically generating
--   the image for each frame then use `False`. If you have loaded it
--   from a file then use `True`.
bitmapOfByteString :: Int -> Int -> BitmapFormat -> ByteString -> Bool -> Picture
bitmapOfByteString width height fmt bs cacheMe
 = unsafePerformIO
 $ do   let len = width * height * 4
        ptr     <- mallocBytes len
        fptr    <- newForeignPtr finalizerFree ptr

        BSU.unsafeUseAsCString bs
         $ \cstr -> copyBytes ptr (castPtr cstr) len

        let bdata = BitmapData len fmt fptr
        return $ Bitmap width height bdata cacheMe
{-# NOINLINE bitmapOfByteString #-}


-- | O(size). Copy a `BMP` file into a bitmap.
bitmapOfBMP :: BMP -> Picture
bitmapOfBMP bmp
 = unsafePerformIO
 $ do   let (width, height)     = bmpDimensions bmp
        let bs                  = unpackBMPToRGBA32 bmp
        let len                 = width * height * 4

        ptr     <- mallocBytes len
        fptr    <- newForeignPtr finalizerFree ptr

        BSU.unsafeUseAsCString bs
         $ \cstr -> copyBytes ptr (castPtr cstr) len

        let bdata = BitmapData len (BitmapFormat BottomToTop PxRGBA) fptr

        return $ Bitmap width height bdata True
{-# NOINLINE bitmapOfBMP #-}


-- | Load an uncompressed 24 or 32bit RGBA BMP file as a bitmap.
loadBMP :: FilePath -> IO Picture
loadBMP filePath
 = do   ebmp    <- readBMP filePath
        case ebmp of
         Left err       -> error $ show err
         Right bmp      -> return $ bitmapOfBMP bmp

