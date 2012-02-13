-- hectare: a functional abstract real time strategy
-- Copyright (C) 2009+ Nicolas Wu
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- We make use of Terrain Simplification Simplified by Lindstrom and Pascucci
{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import           Hectare.Util ((×))
import           Hectare.State (State, initState, terrain)
import qualified Hectare.Camera as Camera
import           Hectare.Camera (Camera)
import           Hectare.Render (renderFrustum)
import           Hectare.Terrain (renderMeshVisible)

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLUT as GLUT

import System.Exit (exitSuccess)

import Data.IORef    (IORef, newIORef, readIORef, writeIORef)
import Data.StateVar (get, ($=), ($~))

-- Initialisers --------------------------------------------------------------

initGL :: IO ()
initGL = do
  -- Scene
  GL.clearColor $= GL.Color4 0 0 0 0
  GL.clearDepth $= 1

  -- Materials
  GL.materialAmbientAndDiffuse GL.Front $= GL.Color4 0.9 0.9 0.9 1
  GL.materialSpecular          GL.Front $= GL.Color4 1 1 1 1
  GL.materialShininess         GL.Front $= 128

  -- Lights
  GL.lighting              $= GL.Enabled
  GL.ambient  (GL.Light 1) $= GL.Color4  0.1 0.1  0.1 1.0
  GL.diffuse  (GL.Light 1) $= GL.Color4  0.9 0.9  0.9 1.0
  GL.specular (GL.Light 1) $= GL.Color4  0.5 0.5  0.5 1.0
  GL.position (GL.Light 1) $= GL.Vertex4 0.0 0.0 20.0 0.0
  GL.light    (GL.Light 1) $= GL.Enabled

  -- Rendering
  GL.shadeModel  $= GL.Flat           -- Flat | Smooth
  GL.polygonMode $= (GL.Fill,GL.Line) -- Point Line Fill, (front, back)
  GL.depthFunc   $= Just GL.Less
  GL.cullFace    $= Just GL.Back
  GL.hint GL.PerspectiveCorrection $= GL.Nicest
  GL.normalize   $= GL.Enabled

  -- Textures
  GL.texture GL.Texture2D $= GL.Enabled

-- Callbacks -----------------------------------------------------------------

display :: IORef Camera -> IORef State -> IO ()
display cameraRef stateRef = do
  camera <- get cameraRef
  state  <- get stateRef
  GL.clear [ GL.DepthBuffer, GL.ColorBuffer ]
  GL.loadIdentity
  Camera.prepare camera
  renderMeshVisible (terrain state) camera
  -- renderMesh (terrain state) camera
  GLUT.Size width height <- get GLUT.windowSize
  renderFrustum (fromIntegral width) (fromIntegral height) (Camera.iris camera)

reshape :: IORef Camera -> GLUT.Size -> IO ()
reshape cameraRef size@(GLUT.Size w h) = do
  GLUT.windowSize $= size

  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  perspective fovy aspect near far
  GL.matrixMode $= GL.Modelview 0
  cameraRef $~ Camera.reshape (fromIntegral w) (realToFrac $ deg2rad fovx)
  -- GLUT.postRedisplay Nothing
 where
  deg2rad x = (pi / 180.0) * x
  rad2deg x = (180.0 / pi) * x
  fovx = 45 :: GL.GLdouble
  fovy = rad2deg (2 * atan (fromIntegral h * tan (deg2rad $ fovx / 2) / fromIntegral w))
  aspect = fromIntegral w / fromIntegral h
  near   = 0.01
  far    = 2000

perspective :: GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> IO ()
perspective fovy aspect near far =
  GL.frustum xmin xmax ymin ymax near far
 where ymax = near * tan (fovy * pi / 360.0)
       ymin = - ymax
       xmax = ymax * aspect
       xmin = ymin * aspect


update :: IORef Camera -> IORef State -> IO ()
update cameraRef _ = do
  cameraRef $~ Camera.update
  GLUT.postRedisplay Nothing


keyboard :: IORef Camera -> IORef State -> GLUT.KeyboardCallback
keyboard cameraRef stateRef char _ = do
  case char of
    'e'   -> cameraRef $~ Camera.setDrift Camera.Forward   speed
    'd'   -> cameraRef $~ Camera.setDrift Camera.Backward  speed
    'f'   -> cameraRef $~ Camera.setDrift Camera.Rightward speed
    's'   -> cameraRef $~ Camera.setDrift Camera.Leftward  speed
    'w'   -> cameraRef $~ Camera.setDrift Camera.Downward  speed
    'r'   -> cameraRef $~ Camera.setDrift Camera.Upward    speed
    'E'   -> cameraRef $~ Camera.setDrift Camera.Forward   0 >>
        cameraRef $~ Camera.setPitch (-angle)
    'D'   -> cameraRef $~ Camera.setDrift Camera.Backward  0 >>
        cameraRef $~ Camera.setPitch angle
    'S'   -> cameraRef $~ Camera.setDrift Camera.Leftward  0 >>
        cameraRef $~ Camera.setPivot (-angle)
    'F'   -> cameraRef $~ Camera.setDrift Camera.Rightward 0 >>
        cameraRef $~ Camera.setPivot angle
    't'   -> GL.texture GL.Texture2D $~ toggleCapability
    'T'   -> GL.polygonMode $~ (togglePolygonMode × id)
    'g'   -> do cameraRef $~ Camera.setTau (-0.1)
                (GLUT.Size width height) <- get GLUT.windowSize
                reshape cameraRef (GLUT.Size width height)
    'G'   -> do cameraRef $~ Camera.setTau 0.1
                (GLUT.Size width height) <- get GLUT.windowSize
                reshape cameraRef (GLUT.Size width height)
    '\27' -> exitSuccess
    _     -> return ()
  update cameraRef stateRef
 where
  speed = 3
  angle = 5

keyboardUp :: IORef Camera -> IORef State -> GLUT.KeyboardCallback
keyboardUp cameraRef stateRef char _ = do
  case char of
    'e'   -> cameraRef $~ Camera.setDrift Camera.Forward   0
    'd'   -> cameraRef $~ Camera.setDrift Camera.Backward  0
    'f'   -> cameraRef $~ Camera.setDrift Camera.Rightward 0
    's'   -> cameraRef $~ Camera.setDrift Camera.Leftward  0
    'w'   -> cameraRef $~ Camera.setDrift Camera.Downward  0
    'r'   -> cameraRef $~ Camera.setDrift Camera.Upward    0
    _     -> return ()
  update cameraRef stateRef

mouse :: IORef Camera -> IORef State -> IORef GLUT.Position -> GLUT.MouseCallback
mouse cameraRef stateRef _ GLUT.WheelUp GLUT.Down _ = do
  cameraRef $~ Camera.setSpeed Camera.Toward 12
  update cameraRef stateRef
mouse cameraRef stateRef _ GLUT.WheelDown GLUT.Down _ = do
  cameraRef $~ Camera.setSpeed Camera.Aftward 12
  update cameraRef stateRef
mouse cameraRef _ mouseRef GLUT.MiddleButton GLUT.Down position = do
  writeIORef mouseRef position
  GLUT.cursor $= GLUT.None
  GLUT.motionCallback $= Just (motion cameraRef position)
mouse _ _ mouseRef GLUT.MiddleButton GLUT.Up _ = do
  GLUT.motionCallback $= Nothing
  position <- readIORef mouseRef
  GLUT.pointerPosition $= position
  GLUT.cursor $= GLUT.Inherit
mouse _ _ _ button state position =
  putStrLn (show button ++ " " ++ show state ++ " " ++ show position)

motion :: IORef Camera -> GLUT.Position -> GLUT.Position -> IO ()
motion cameraRef position@(GLUT.Position x y) (GLUT.Position x' y') = do
  GLUT.pointerPosition $= position
  let dx = x' - x
      dy = y' - y
  cameraRef $~ Camera.setPivot  (0.1 * fromIntegral dx)
  cameraRef $~ Camera.setPitch  (0.1 * fromIntegral dy)


-- Misc ---------------------------------------------------------------------

toggleCapability :: GL.Capability -> GL.Capability
toggleCapability GL.Enabled = GL.Disabled
toggleCapability GL.Disabled = GL.Enabled

togglePolygonMode :: GL.PolygonMode -> GL.PolygonMode
togglePolygonMode GL.Fill  = GL.Line
togglePolygonMode GL.Line  = GL.Point
togglePolygonMode GL.Point = GL.Fill

-- Main ----------------------------------------------------------------------
main :: IO ()
main = do
  GLUT.initialWindowSize $= GLUT.Size 800 600
  (_, args) <- GLUT.getArgsAndInitialize
  GLUT.initialDisplayMode $= [ GLUT.DoubleBuffered ]
  GLUT.initialDisplayCapabilities $=
    [ GLUT.With  GLUT.DisplayRGB
    , GLUT.With  GLUT.DisplayDouble
    , GLUT.Where GLUT.DisplayDepth GLUT.IsAtLeast 16
    , GLUT.Where GLUT.DisplayRed GLUT.IsEqualTo 8
    , GLUT.Where GLUT.DisplayGreen GLUT.IsEqualTo 8
    , GLUT.Where GLUT.DisplayBlue GLUT.IsEqualTo 8
    ]
  _ <- GLUT.createWindow "hectare"

  state <- case args of
    [textureFile]              -> initState textureFile Nothing
    [textureFile, terrainFile] -> initState textureFile (Just terrainFile)
    _                          -> error
      "Hectare, Copyright (C) 2009, 2010, 2011, 2012 Nicolas Wu\n\
      \Creates a terrain from the first ITEM and renders it \n\
      \  using texture from the second ITEM.\n\
      \Example: hectare data/terrain.tga data/texture.tga"
  stateRef  <- newIORef state
  cameraRef <- newIORef Camera.initCamera
  mouseRef  <- newIORef (GLUT.Position 0 0)

  GLUT.reshapeCallback       $= Just (reshape cameraRef)
  GLUT.keyboardCallback      $= Just (keyboard cameraRef stateRef)
  GLUT.keyboardUpCallback    $= Just (keyboardUp cameraRef stateRef)
  GLUT.mouseCallback         $= Just (mouse cameraRef stateRef mouseRef)
  GLUT.motionCallback        $= Nothing
  GLUT.displayCallback       $= do display cameraRef stateRef
                                   update cameraRef stateRef
                                   GLUT.swapBuffers
  GLUT.specialCallback       $= Nothing
  GLUT.specialUpCallback     $= Nothing

  initGL

  GLUT.mainLoop
