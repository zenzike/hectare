module Hectare.Image where

import Data.Word
import Graphics.Rendering.OpenGL (PixelData(..), TextureSize2D(..))

import Hectare.Terrain.VectorTerrain

class ImageTexture a where
  imagePixelData :: a -> IO (PixelData Word8)
  imageSize      :: a -> TextureSize2D

class ImageTerrain a where
  imageTerrain :: a -> VectorTerrain
