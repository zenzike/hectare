module Hectare.State where

import Hectare.Image
import Hectare.Image.TGA (ImageTGA)
import Hectare.Terrain.VectorTerrain
import Hectare.Terrain.PerlinTerrain
import Hectare.Render (Render (..))

import qualified Graphics.Rendering.OpenGL as GL
import Data.Binary (decodeFile)

import Data.IORef (IORef, newIORef)
import Control.Monad (liftM)
import Data.StateVar (($=))

data State = State {
  displayList :: GL.DisplayList,
  terrain :: VectorTerrain,
  lastDrawTime :: IORef Int
}

instance Render State where
  render state = GL.callList (displayList state)

initState :: FilePath -> Maybe FilePath -> IO State
initState textureFile mTerrainFile = do
  initTerrain <- case mTerrainFile of
    Just terrainFile -> do
      terrainImage <- decodeFile terrainFile :: IO ImageTGA
      return $ imageTerrain terrainImage
    Nothing -> return $ convert 32 $ PerlinTerrain 8 512
  initDisplayList <- GL.defineNewList GL.Compile $ return ()
  initTime <- newIORef 0
    -- renderPrimitive TriangleStrip $
    --   texturedNormalsTriangleStrip
    --   (Vector3 1 1 1) (Vertex3 0 0 0) (Vertex3 0 0 0)
    --   (myPath (terrainToSurface $ initTerrain))
      --(myPath2 (imageTerrain terrainImage))

  -- Texture
  texImage <- decodeFile textureFile :: IO ImageTGA
  tex      <- liftM head (GL.genObjectNames 1)

  GL.textureBinding GL.Texture2D $= Just tex
  GL.textureFilter  GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  pd <- imagePixelData texImage
  GL.texImage2D Nothing GL.NoProxy 0  GL.RGB' (imageSize texImage) 0 pd
  GL.matrixMode $= GL.Texture
  GL.loadIdentity
  GL.scale
    (1 / (fromIntegral $ xsize initTerrain :: GL.GLfloat))
    (1 / fromIntegral (ysize initTerrain)) 1

  return $ State initDisplayList initTerrain initTime
