module Hectare.Render where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (GLfloat)

import Data.StateVar (($=))

class Render a where
  render :: a -> IO ()

texturedTriangleStrip :: Bool -> GL.Vertex3 GLfloat
  -> GL.Vertex3 GLfloat -> [GL.Vertex3 GLfloat] -> IO ()
texturedTriangleStrip _ _ _ [] = return ()
texturedTriangleStrip parity u v (w@(GL.Vertex3 wx wy _):ws) = do
  GL.normal $ crossNorm parity u v w
  GL.texCoord (GL.TexCoord2 wx wy)
  GL.vertex w
  texturedTriangleStrip (not parity) v w ws

normalsTriangleStrip :: Bool -> GL.Vertex3 GLfloat
  -> GL.Vertex3 GLfloat -> [GL.Vertex3 GLfloat] -> IO ()
normalsTriangleStrip _ _ _ [] = return ()
normalsTriangleStrip parity u v (w:ws) = do
  GL.normal $ crossNorm parity u v w
  GL.vertex w
  normalsTriangleStrip (not parity) v w ws

{-# INLINE crossNorm #-}
crossNorm :: (Num a) => Bool -> GL.Vertex3 a -> GL.Vertex3 a -> GL.Vertex3 a -> GL.Normal3 a
crossNorm parity (GL.Vertex3 ux uy uz) (GL.Vertex3 vx vy vz) (GL.Vertex3 wx wy wz) =
  GL.Normal3 nx ny nz
 where
  ax = if parity then vx - ux else ux - vx
  ay = if parity then vy - uy else uy - vy
  az = if parity then vz - uz else uz - vz
  bx = wx - ux
  by = wy - uy
  bz = wz - uz
  nx = ay * bz - az * by
  ny = az * bx - ax * bz
  nz = ax * by - ay * bx

type Frustum = [GL.Plane GLfloat]

renderFrustum :: Int -> Int -> GLfloat -> IO ()
renderFrustum width height size =
  GL.preservingMatrix $ do
    GL.loadIdentity
    GL.translate $ GL.Vector3 (0.375 :: GLfloat) 0.375 0
    GL.matrixMode GL.$= GL.Projection
    GL.preservingMatrix $ do
      GL.loadIdentity
      GL.ortho 0 (fromIntegral width) 0 (fromIntegral height) (-1) 1
      GL.matrixMode GL.$= GL.Modelview 0
      GL.preservingAttrib [GL.EnableAttributes] $ do
        GL.lighting GL.$= GL.Disabled
        GL.texture GL.Texture2D GL.$= GL.Disabled
        GL.color $ GL.Color3 (1 :: GLfloat) 0 0
        GL.depthFunc $= Nothing
        GL.renderPrimitive GL.LineLoop $ do
          let xmin = 0.5 * (1.0 - size) * fromIntegral width
          let ymin = 0.5 * (1.0 - size) * fromIntegral height
          let xmax = 0.5 * (1.0 + size) * fromIntegral width
          let ymax = 0.5 * (1.0 + size) * fromIntegral height
          GL.vertex $ GL.Vertex2 xmin ymin
          GL.vertex $ GL.Vertex2 xmax ymin
          GL.vertex $ GL.Vertex2 xmax ymax
          GL.vertex $ GL.Vertex2 xmin ymax

getFrustum :: GLfloat -> IO Frustum
getFrustum iris = do
  GL.matrixMode GL.$= GL.Projection
  clipMatrix <- GL.unsafePreservingMatrix $ do
    modelViewMatrix <- GL.get . GL.matrix . Just $
      GL.Modelview 0 :: IO (GL.GLmatrix GLfloat)
    GL.multMatrix modelViewMatrix
    GL.get $ GL.matrix Nothing :: IO (GL.GLmatrix GLfloat)
  [c00,c01,c02,c03,
   c10,c11,c12,c13,
   c20,c21,c22,c23,
   c30,c31,c32,c33] <- GL.getMatrixComponents GL.ColumnMajor clipMatrix
  let sc03 = iris * c03
  let sc13 = iris * c13
  let sc23 = iris * c23
  let sc33 = iris * c33
  GL.matrixMode $= GL.Modelview 0
  return $ map norm
    [ negPlane sc03 c00 sc13 c10 sc23 c20 sc33 c30
    , posPlane sc03 c00 sc13 c10 sc23 c20 sc33 c30
    , negPlane sc03 c01 sc13 c11 sc23 c21 sc33 c31
    , posPlane sc03 c01 sc13 c11 sc23 c21 sc33 c31
    , negPlane c03 c02 c13 c12 c23 c22 c33 c32
    , posPlane c03 c02 c13 c12 c23 c22 c33 c32
    ]
 where
  negPlane x0 x1 y0 y1 z0 z1 d0 d1 =
    GL.Plane (x0-x1) (y0-y1) (z0-z1) (d0-d1)
  posPlane x0 x1 y0 y1 z0 z1 d0 d1 =
    GL.Plane (x0+x1) (y0+y1) (z0+z1) (d0+d1)
  norm (GL.Plane x y z d) = GL.Plane (x/h) (y/h) (z/h) (d/h)
   where
    h = sqrt (x*x + y*y + z*z)


-- Frustum Culling with Hessian Plane Equations
--   A plane equation is normally of the form
--  a*x + b*y + c*z + d = 0
--   The Hessian normal form of plane equation is made up
--   of a normalised vector, along with a constant:
--  nx*x + ny*y + nz*z + nd = 0
--     where
--      nx = a / nh
--      ny = b / nh
--      nz = c / nh
--      nd = d / nh
--      nh = sqrt (a*a + b*b + c*c)
--
-- Now we need extract the frustum plane equations from the scene
-- This can be done by multiplying the model and projection matrices

-- We can also extract the plane equations from the camera.
