{-# LANGUAGE Rank2Types #-}

module Hectare.Terrain.VectorTerrain where

import qualified Hectare.Terrain as T
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (GLfloat)

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM

import Control.Monad.ST (ST)
import Control.Monad (forM_)

import Debug.Trace

-- nodes hold the values (z, ∊, r)
data VectorTerrain = VectorTerrain
  { xsize :: Int
  , ysize :: Int
  , nodes :: V.Vector (Float, Float, Float)
  }

instance T.Terrain VectorTerrain where
  xsize = xsize
  ysize = ysize
  nodes terrain x y = nodes terrain V.! (xsize terrain * y + x)

-- holds d, ∊, and r.
instance Show VectorTerrain where
  show (VectorTerrain x y _) = "Terrain " ++ show x ++ "x" ++ show y

convert :: T.Terrain t => Float -> t -> VectorTerrain
convert scale terrain = computeErrors $! VectorTerrain (T.xsize terrain) (T.ysize terrain) $
  V.fromList $ map (\(z,_,_) -> (z*scale,0,0)) [T.nodes terrain x y | y <- [0 .. T.ysize terrain - 1], x <- [0 .. T.xsize terrain - 1]]

computeErrors :: VectorTerrain -> VectorTerrain
computeErrors terrain = terrain { nodes = nodes' }
 where
  nodes' = V.modify (inject size (T.depth terrain)) (nodes terrain)
  inject mx m v =
    forM_ [1 .. m] $ \d -> blackErrors mx d v >> whiteErrors mx d v
  size = max (xsize terrain) (ysize terrain)


-- We compute the errors of black and white vertices, as in
-- the following example:
--
--   .B.B.  ..... ..B.. .....
--   B.B.B  .W.W. ..... .....
--   .B.B.  ..... B...B ..W..
--   B.B.B  .W.W. ..... .....
--   .B.B.  ..... ..B.. .....
--
-- Each diagram is represented by [(Int, Int)],
-- and we have a list `xss` of these, where `leaves = head xss`.
-- The above example is produced by:
--   allErrors 2 2

-- allErrors :: Int -> Int -> [[(Int, Int)]]
-- allErrors m 0 = []
-- allErrors m d = blackErrors m (m-d+1) : whiteErrors m (m-d+1) : allErrors m (d-1)

-- blackErrors
--  mx, my : The max x and y on the board in actual values (no squaring)
--  m : 2^m is the maximum distance, which gives us the board range
--  d : 2^m is the current distance
--  v : the terrain vector v
--
--  When d is 1, we must be at leaves
blackErrors :: Int -> Int -> (forall s. V.MVector s (Float, Float, Float) -> ST s ())
blackErrors mx d v = do
  forM_ [(x, y) | x <- [c2, c2 + c .. mx-c2], y <- [0, c .. mx-1]] $ \(x,y) -> do
    (zi, _, _) <- VM.read v (mx*y+x)
    (zl, _, _) <- VM.read v (mx*y+x+c2)
    (zr, _, _) <- VM.read v (mx*y+x-c2)

    let cs = blackChildren mx (x,y) d
    ncs <- mapM (VM.read v . (\(i,j) -> mx*j+i)) cs

    let ei = maximum $ einc zi zl zr : map (\(_,ec,_) -> ec) ncs
    let ri = if d == 1 then 0.0 else maximum (map (\(zc,_,rc) -> sqrt (dist2 zi zc) + rc) ncs)

    VM.write v (mx * y + x) (zi, ei, ri)

  forM_ [(x, y) | x <- [0, c .. mx-1], y <- [c2, c2 + c .. mx-c2]] $ \(x,y) -> do
    (zi, _, _) <- VM.read v (mx*y+x)
    (zl, _, _) <- VM.read v (mx*(y+c2)+x)
    (zr, _, _) <- VM.read v (mx*(y-c2)+x)

    let cs = blackChildren mx (x,y) d
    ncs <- mapM (VM.read v . (\(i,j) -> mx*j+i)) cs

    let ei = maximum $ einc zi zl zr : map (\(_,ec,_) -> ec) ncs
    let ri = if d == 1 then 0.0 else maximum (map (\(zc,_,rc) -> sqrt (dist2 zi zc) + rc) ncs)

    VM.write v (mx * y + x) (zi, ei, ri)
 where
  einc zi zl zr = abs (zi - (zl+zr)/2)
  c  = 2^d
  c2 = 2^(d-1)
  c4 = 2^(d-2)
  dist2 zi zc = ((zi - zc)^(2::Int)) + 2 * (c4^(2::Int))

whiteErrors :: Int -> Int -> (forall s. V.MVector s (Float, Float, Float) -> ST s ())
whiteErrors mx d v =
  forM_ [(x, y) | x <- [c2, c2 + c .. mx-c2], y <- [c2, c2 + c .. mx-c2]] $ \(x,y) -> do
    (zi, _, _) <- VM.read v (mx*y+x)
    (zl, _, _) <- VM.read v (mx*(y+c2)+x+c2)  -- a quick approximation
    (zr, _, _) <- VM.read v (mx*(y-c2)+x-c2)

    let cs = whiteChildren mx (x,y) d
    ncs <- mapM (VM.read v . (\(i,j) -> mx*j+i)) cs

    let ei = maximum $ einc zi zl zr : map (\(_,ec,_) -> ec) ncs
    let ri = maximum (map (\(zc,_,rc) -> sqrt (dist2 zi zc) +rc) ncs)

    VM.write v (mx*y+x) (zi, ei, ri)
 where
  einc zi zl zr = abs (zi - (zl+zr)/2)
  c  = 2^d
  c2 = 2^(d-1)
  dist2 zi zc = ((zi - zc)^(2::Int)) + (fromIntegral c2 ^ (2::Int))

-- Computes the children of a node at a given distance
blackChildren :: Int -> (Int, Int) -> Int -> [(Int, Int)]
blackChildren _ _ 1 = []
blackChildren mx (x,y) d =
  filter (\(u,v) -> u > 0 && u < mx && v > 0 && v < mx)
    [(x+c,y+c), (x+c,y-c), (x-c,y+c), (x-c,y-c)]
 where
  c = 2^(d-2)

whiteChildren :: Int -> (Int, Int) -> Int -> [(Int, Int)]
whiteChildren mx (x,y) d =
  filter (\(u,v) -> u > 0 && u < mx && v > 0 && v < mx)
    [(x,y+c), (x+c,y), (x,y-c), (x-c,y)]
 where
  c = 2^(d-1)

mySamples :: GLfloat
mySamples = 64

terrainToSurface :: VectorTerrain -> [[GL.Vertex3 GLfloat]]
terrainToSurface terrain =
  [[GL.Vertex3 (fromIntegral x)
            (fromIntegral y)
            (realToFrac . (\(z,_,_) -> z) $ nodes terrain V.! (xsize terrain*y+x)) |
            y <- [0 .. fromIntegral $ xsize terrain - 1]] |
            x <- [0 .. fromIntegral $ ysize terrain - 1]]

