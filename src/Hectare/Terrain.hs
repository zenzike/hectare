module Hectare.Terrain where

import qualified Graphics.Rendering.OpenGL as GL

import Hectare.Camera
import Hectare.Render (Frustum, getFrustum, texturedTriangleStrip)
import Hectare.Util   (log2)

import Control.Monad       (when, forM)
import Control.Monad.State (State, get, put, execState)

import Unsafe.Coerce (unsafeCoerce)
import Data.StateVar (($=))

class Terrain t where
  xsize :: t -> Int
  ysize :: t -> Int
  nodes :: t -> Int -> Int -> (Float, Float, Float)

depth :: Terrain t => t -> Int
depth terrain = log2 $ size - 1
 where
  size = max (xsize terrain) (ysize terrain)

renderMesh :: Terrain t => t -> Camera -> IO ()
renderMesh terrain camera =
  GL.renderPrimitive GL.TriangleStrip $
    texturedTriangleStrip True (GL.Vertex3 0 0 0) (GL.Vertex3 0 0 0) $
      map (\(x,y,z) ->
        GL.Vertex3 (fromIntegral x) (fromIntegral y) (unsafeCoerce z))
        (meshRefine terrain camera)

renderMeshVisible :: Terrain t => t -> Camera -> IO ()
renderMeshVisible terrain camera = do
  frustum <- getFrustum (iris camera)
  GL.renderPrimitive GL.TriangleStrip $
    texturedTriangleStrip True (GL.Vertex3 0 0 0) (GL.Vertex3 0 0 0) $
      map (\(x,y,z) ->
        GL.Vertex3 (fromIntegral x) (fromIntegral y) (unsafeCoerce z))
        (meshRefineVisible frustum terrain camera)

meshRefine :: Terrain t => t -> Camera -> [(Int, Int, Float)]
meshRefine terrain camera =
  tail . reverse . fst . flip execState ([extend sw, extend sw], False) $
    forM [(sw, se), (se, ne), (ne, nw), (nw, sw)] $ \(j,k) -> do
      submeshRefine j (extend a) k n terrain camera
      append (extend k) True
 where
  n  = 2 * depth terrain
  a  = (xmid,ymid)
  sw = (xmin,ymin)
  se = (xmax,ymin)
  ne = (xmax,ymax)
  nw = (xmin,ymax)
  xmin = 0
  ymin = 0
  xmax = size - 1
  ymax = size - 1
  xmid = (xmin + xmax) `div` 2
  ymid = (ymin + ymax) `div` 2
  extend (x,y) = (x, y, (\(z,_,_) -> z) $ nodes terrain x y)
  size = max (xsize terrain) (ysize terrain)

submeshRefine :: Terrain t => (Int, Int) -> (Int, Int, Float) -> (Int, Int) -> Int
  -> t -> Camera -> State ([(Int, Int, Float)], Bool) ()
submeshRefine l@(lx,ly) a@(ax,ay,_) r@(rx,ry) d terrain camera = do
  when refine $ submeshRefine l (mx,my,mz) (ax,ay) d' terrain camera
  append a (odd d)
  when refine $ submeshRefine (ax,ay) (mx,my,mz) r d' terrain camera
 where
  refine = d > 1 && mactive
  (mactive, mz) = active (mx,my) terrain camera
  (mx,my)       = ((lx + rx) `div` 2, (ly + ry) `div` 2)
  d'            = d-1

submeshRefineVisible :: Terrain t => (Int, Int) -> (Int, Int, Float) -> (Int, Int) -> Int ->
  Frustum -> t -> Camera -> State ([(Int, Int, Float)], Bool) ()
submeshRefineVisible l a r d [] terrain camera = submeshRefine l a r d terrain camera
submeshRefineVisible l@(lx,ly) a@(ax,ay,_) r@(rx,ry) d ps terrain camera = do
  when refine $ submeshRefineVisible l (mx,my,mz) (ax,ay) d' ps' terrain camera
  append a (odd d)
  when refine $ submeshRefineVisible (ax,ay) (mx,my,mz) r d' ps' terrain camera
 where
  refine            = d > 1 && mactive && mvisible
  (mactive, mz, mr) = active2 (mx,my) terrain camera
  (mvisible, ps')   = visible (mx,my,mz) mr ps
  (mx,my)           = ((lx + rx) `div` 2, (ly + ry) `div` 2)
  d'                = d-1

active2 :: Terrain t => (Int, Int) -> t -> Camera -> (Bool, Float, Float)
active2 (x,y) terrain camera =
  (sqr (nu camera * e + r) > d, z, r)
 where
    sqr v = v * v
    d = dist2 x y z camera
    (z, e, r) = nodes terrain x y

visible :: (Int, Int, Float) -> Float -> Frustum -> (Bool, Frustum)
visible _ _ [] = (True, [])
visible p@(px,py,pz) r (n@(GL.Plane nx ny nz nd):ns)
  | s < -r      = (False, n:ns)
  | s > r     = (b,     ns')
  | otherwise  = (b,     n:ns')
 where
  s = unsafeCoerce nx * fromIntegral px +
      unsafeCoerce ny * fromIntegral py +
      unsafeCoerce nz * pz +
      unsafeCoerce nd
  (b,ns') = visible p r ns

meshRefineVisible :: Terrain t => Frustum -> t -> Camera -> [(Int, Int, Float)]
meshRefineVisible frustum terrain camera =
  tail . reverse . fst . flip execState ([extend sw, extend sw], False) $
    forM [(sw, se), (se, ne), (ne, nw), (nw, sw)] $ \(j,k) -> do
      submeshRefineVisible j (extend a) k n frustum terrain camera
      append (extend k) True
 where
  n  = 2 * depth terrain
  a  = (xmid,ymid)
  sw = (xmin,ymin)
  se = (xmax,ymin)
  ne = (xmax,ymax)
  nw = (xmin,ymax)
  xmin = 0
  ymin = 0
  xmax = size - 1
  ymax = size - 1
  xmid = (xmin + xmax) `div` 2
  ymid = (ymin + ymax) `div` 2
  extend (x,y) = (x, y, (\(z,_,_) -> z) $
    nodes terrain x y)
  size = max (xsize terrain) (ysize terrain)

append :: Eq a => a -> Bool -> State ([a], Bool) ()
append v p = do
  (vs@(v0:v1:_), parity) <- get
  when (v /= v0 && v /= v1) $
    if p /= parity
      then put (v:vs,    p)
      else put (v:v1:vs, p)

{-# INLINE active #-}
active :: Terrain t => (Int, Int) -> t -> Camera -> (Bool, Float)
active (x,y) terrain camera =
  (sqr (nu camera * e + r) > d, z)
 where
    sqr v = v * v
    d = dist2 x y z camera
    (z, e, r) = nodes terrain x y

{-# INLINE dist2 #-}
dist2 :: Int -> Int -> Float -> Camera -> Float
dist2 tx ty tz camera =
  sqr (fromIntegral tx - unsafeCoerce cx) +
  sqr (fromIntegral ty - unsafeCoerce cy) +
  sqr (tz - unsafeCoerce cz)
 where
  sqr x = x * x
  GL.Vector3 cx cy cz = pos camera
