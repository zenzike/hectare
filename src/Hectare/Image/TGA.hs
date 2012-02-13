-- This module defines the TGA file format for importation. Only the mandatory fields are fully implemented.
module Hectare.Image.TGA where

import Graphics.Rendering.OpenGL ( PixelData(..), PixelFormat(..), TextureSize2D(..), 
                                   DataType(..) )

import Hectare.Image
import Hectare.Terrain.VectorTerrain (VectorTerrain(VectorTerrain), computeErrors)
import Data.Binary
import Data.Binary.Get

--import Data.Map(Map, (!), fromList)
-- import Data.Array.Unboxed
import qualified Data.Vector.Unboxed as V

import Foreign (mallocBytes, pokeByteOff)

import Data.Bits (clearBit, testBit)

import qualified Data.ByteString.Lazy as LB

data ImageTGA = ImageTGA {
  imageType   :: TGAImageType,
  compressed  :: Bool,
  bpp         :: Int,
  xsize       :: Word16,
  ysize       :: Word16,
  colorData   :: Maybe LB.ByteString,
  imageData   :: LB.ByteString
  }

data TGAImageType = Null | ColorMap | TrueColor | MonoColor

instance Show ImageTGA where
  show image =
    "TGA " ++ show (xsize image) ++ "x" ++ show (ysize image)

instance Binary ImageTGA where
  get = do
    -- Field 1
    idLength  <- getWord8  -- # of bytes in Field 6, 0 -> no image id
    -- Field 2
    colorMap' <- getWord8  -- 0 | 1
    -- Field 3
    imageTypeByte <- getWord8  -- 0 | 1 | 2 | 3 | 9 | 10 | 11 (bit 3=RLE)
    let imageType' =
          case (clearBit imageTypeByte 3) of
            0 -> Null
            1 -> ColorMap
            2 -> TrueColor
            3 -> MonoColor
            _ -> error $ "Invalid image: unknown imageType: " ++ show imageTypeByte
    let compressed' = testBit imageTypeByte 3
    -- Field 4
    _ <- getWord16le --firstEntryindex <- getWord16le
    colorMapLength  <- getWord16le
    colorMapSize    <- getWord8 -- 15 | 16 | 24 | 32
    let colorMapBytes =
          case colorMapSize of
            15 -> 2
            16 -> 2
            24 -> 3
            32 -> 4
            _  -> error $ "Invalid image: unknown color map size: " ++ show colorMapSize
    -- Field 5
    _ <- getWord16le -- xOrigin         <- getWord16le -- we ignore this
    _ <- getWord16le -- yOrigin         <- getWord16le -- we ignore this
    xsize'          <- getWord16le
    ysize'          <- getWord16le
    pixelDepth      <- getWord8 -- 8 | 16 | 24 | 32
    let bpp' =
          case pixelDepth of
            8  -> 1
            16 -> 2
            24 -> 3
            32 -> 4
            _  -> error $ "Invalid image: unknown pixel depth: " ++ show pixelDepth
    _ <- getWord8 -- imageDescriptor <- getWord8 -- image descriptor and origin
    -- Field 6
    _ <- getLazyByteString $ fromIntegral idLength -- imageId   <- getLazyByteString $ fromIntegral idLength
    -- Field 7 -- color data
    colorData' <-  case colorMap' of
      0 -> return Nothing
      1 -> do cd <- getLazyByteString (fromIntegral colorMapLength * fromInteger colorMapBytes ) -- Color map
              return $ Just cd
        -- 4.3
      _ -> fail $ "Invalid image: unknown color map type" ++ show colorMap'
    -- Field 8 -- image data
    pixelData' <- getLazyByteString (fromIntegral xsize' * fromIntegral ysize' * fromIntegral bpp')
    return $ ImageTGA imageType' compressed' bpp' xsize' ysize' colorData' pixelData'
  put = error "No put definition for tga images!"

instance ImageTexture ImageTGA where
  imagePixelData image = do
    let (pixelFormat, dataType) =
          case bpp image of
            1 -> (Alpha, UnsignedByte)
            2 -> (RGBA,  UnsignedShort1555Rev)
            3 -> (BGR,   UnsignedByte)
            4 -> (RGBA,  UnsignedInt8888)
            _  -> error $ "Invalid image: unknown bpp: " ++ show (bpp image)
    bytes <- mallocBytes (bpp image * x * y)
    mapM_ (uncurry $ pokeByteOff bytes) $ zip [0 .. bpp image * x * y] (LB.unpack $ imageData image)
    return (PixelData pixelFormat dataType bytes)
    where
      x = fromIntegral $ xsize image
      y = fromIntegral $ ysize image
  imageSize image =
    TextureSize2D (fromIntegral (xsize image)) (fromIntegral (ysize image))

instance ImageTerrain ImageTGA where
  imageTerrain image =  computeErrors $
    case bpp image of
      1 ->
        VectorTerrain
          (fromIntegral $ xsize image)
          (fromIntegral $ ysize image) $
          (V.fromList $ map ((\z -> (z, 0, 0)) . (/ 8) . fromIntegral)
            (LB.unpack (imageData image))) -- z size ranges to 256
            V.++  V.replicate (fromIntegral $
              size*size - (fromIntegral . LB.length . imageData $ image)) (0,0,0)
      _ -> error $ "Invalid image: cannot build image terrain from bpp: " ++ show (bpp image)
   where
    size = max (xsize image) (ysize image)

