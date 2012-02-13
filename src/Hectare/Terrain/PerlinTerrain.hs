module Hectare.Terrain.PerlinTerrain where

-- Resources:
-- http://www.programmersheaven.com/2/perlin
-- http://www.gamasutra.com/view/feature/3098/a_realtime_procedural_universe_.php
-- http://mrl.nyu.edu/~perlin/noise/
import qualified Hectare.Terrain as T
import Data.Bits ((.&.))

data PerlinTerrain = PerlinTerrain
  { depth :: Int
  , width :: Int
  }

instance T.Terrain PerlinTerrain where
  xsize terrain = 1 + width terrain
  ysize terrain = 1 + width terrain
  nodes terrain x y = (z, 0, 0)
   where
    i = fromIntegral x / (fromIntegral $ width terrain `div` 2)
    j = fromIntegral y / (fromIntegral $ width terrain `div` 2)
    z = 1 + brownian2d (i, j) (depth terrain)


noise2d :: (Float, Float) -> Float
noise2d (x,y) =
  lerp v
    (lerp u
      (gradient (perm !! ((perm !! i0) + j0)) x0 y0)
      (gradient (perm !! ((perm !! i1) + j0)) x1 y0))
    (lerp u
      (gradient (perm !! ((perm !! i0) + j1)) x0 y1)
      (gradient (perm !! ((perm !! i1) + j1)) x1 y1))
 where
  u = fade x0
  v = fade y0
  i0 = floor x .&. 255
  j0 = floor y .&. 255
  i1 = i0 + 1
  j1 = j0 + 1
  x0 = x - (fromIntegral $ floor x)
  y0 = y - (fromIntegral $ floor y)
  x1 = x0 - 1
  y1 = y0 - 1


fade :: Float -> Float
fade t = t * t * t * (t * (t * 6 - 15) + 10)

lerp :: Float -> Float -> Float -> Float
lerp t a b = a + t * (b - a)

gradient :: Int -> Float -> Float -> Float
gradient hash i j = case hash .&. 3 of
  0 ->  i + j
  1 -> -i + j
  2 ->  i - j
  3 -> -i - j

perm :: [Int]
perm = (\p -> p ++ p)
  [ 151,160,137, 91, 90, 15,131, 13,201, 95, 96, 53,194,233,  7,225
  , 140, 36,103, 30, 69,142,  8, 99, 37,240, 21, 10, 23,190,  6,148
  , 247,120,234, 75,  0, 26,197, 62, 94,252,219,203,117, 35, 11, 32
  ,  57,177, 33, 88,237,149, 56, 87,174, 20,125,136,171,168, 68,175
  ,  74,165, 71,134,139, 48, 27,166, 77,146,158,231, 83,111,229,122
  ,  60,211,133,230,220,105, 92, 41, 55, 46,245, 40,244,102,143, 54
  ,  65, 25, 63,161,  1,216, 80, 73,209, 76,132,187,208, 89, 18,169
  , 200,196,135,130,116,188,159, 86,164,100,109,198,173,186,  3, 64
  ,  52,217,226,250,124,123,  5,202, 38,147,118,126,255, 82, 85,212
  , 207,206, 59,227, 47, 16, 58, 17,182,189, 28, 42,223,183,170,213
  , 119,248,152,  2, 44,154,163, 70,221,153,101,155,167, 43,172,  9
  , 129, 22, 39,253, 19, 98,108,110, 79,113,224,232,178,185,112,104
  , 218,246, 97,228,251, 34,242,193,238,210,144, 12,191,179,162,241
  ,  81, 51,145,235,249, 14,239,107, 49,192,214, 31,181,199,106,157
  , 184, 84,204,176,115,121, 50, 45,127,  4,150,254,138,236,205, 93
  , 222,114, 67, 29, 24, 72,243,141,128,195, 78, 66,215, 61,156,180
  ]

brownian2d :: (Float, Float) -> Int -> Float
brownian2d (p,q) 0 = noise2d (p,q)
brownian2d (p,q) n = (1/m) * noise2d (m*p,m*q) + brownian2d (p,q) (n-1)
 where
  m = 2^n

