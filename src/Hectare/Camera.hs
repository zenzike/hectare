module Hectare.Camera where

import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (GLfloat)

import Data.Map (Map, insert, foldWithKey, empty)

data Camera = Camera
  { pos   :: GL.Vector3 GLfloat  -- The position of the camera
  , pitch :: GLfloat             -- The angle facing downwards
  , pivot :: GLfloat             -- The angle clockwise viewed from top
  , delta :: GL.Vector3 GLfloat  -- Forward motion of the camera
  , drift :: Map Motion GLfloat

  , nu       :: Float            -- projection error
  , tau      :: Float            -- tolerance
  , iris     :: GLfloat          -- iris size
  }

data Motion
  = Forward
  | Backward
  | Leftward
  | Rightward
  | Upward
  | Downward
  | Toward
  | Aftward
  deriving (Eq, Ord)

initCamera :: Camera
initCamera = Camera
  (GL.Vector3 0 0 50)
  (-70) 45
  (GL.Vector3 0 0 0)
  empty
  ((pi / 4) * (4 / 800)) 4 0.1

update :: Camera -> Camera
update camera = camera
  { pos = GL.Vector3 (x+(dx*sin ψ)+(dy*cos ψ))
                     (y+(dx*cos ψ)-(dy*sin ψ))
                     (z+dz)
  }
 where
  ψ = pivot camera * pi / 180
  GL.Vector3 x y z    = pos camera
  GL.Vector3 dx dy dz = delta $ foldWithKey setSpeed camera (drift camera)

-- reshape
--  w       Width in pixels
--  fovx    fov of x in radians
--  camera
reshape :: Int -> Float -> Camera -> Camera
reshape w fovx camera = camera
  { nu = if kappa > 0 then 1 / kappa else 10000 }
 where
  kappa = (tau camera / fromIntegral w) * fovx

setDrift :: Motion -> GLfloat -> Camera -> Camera
setDrift motion speed camera = camera { drift = insert motion speed $ drift camera }

setSpeed :: Motion -> GLfloat -> Camera -> Camera
setSpeed motion speed camera =
  case motion of
    Forward   -> camera { delta = GL.Vector3 (dx+speed) dy dz }
    Backward  -> camera { delta = GL.Vector3 (dx-speed) dy dz }
    Leftward  -> camera { delta = GL.Vector3 dx (dy-speed) dz }
    Rightward -> camera { delta = GL.Vector3 dx (dy+speed) dz }
    Upward    -> camera { delta = GL.Vector3 dx dy (dz+speed) }
    Downward  -> camera { delta = GL.Vector3 dx dy (dz-speed) }
    Toward    -> camera { pos = GL.Vector3 (x-(speed*sin ψ*sin θ))
                                           (y-(speed*cos ψ*sin θ))
                                           (z-(speed*cos θ))
                        }
    Aftward   -> camera { pos = GL.Vector3 (x+(speed*sin ψ*sin θ))
                                           (y+(speed*cos ψ*sin θ))
                                           (z+(speed*cos θ))
                        }
 where
  ψ = pivot camera * pi / 180
  θ = pitch camera * pi / 180
  GL.Vector3 x  y  z  = pos camera
  GL.Vector3 dx dy dz = delta camera

prepare :: Camera -> IO ()
prepare camera = do
  GL.rotate (pitch camera) $ GL.Vector3 1 0 0
  GL.rotate (pivot camera) $ GL.Vector3 0 0 1
  GL.translate             $ GL.Vector3 (-x) (-y) (-z)
 where
  GL.Vector3 x y z = pos camera

setPivot :: GLfloat -> Camera -> Camera
setPivot angle camera = camera { pivot = pivot' }
 where
  pivot' = pivot camera + angle

setPitch :: GLfloat -> Camera -> Camera
setPitch angle camera = camera
  { pitch = pitch' }
 where
  pitch' = ((pitch camera + angle) `min` 0) `max` (-180)

setTau :: Float -> Camera -> Camera
setTau dtau camera = camera
  { tau = max 0 (tau camera + dtau) }
