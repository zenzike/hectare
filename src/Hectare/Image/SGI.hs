{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Hectare.Image.SGI (
  ImageSGI(..),
  emptyImageSGI,
  channelsSGI
  )
where

import Hectare.Image
import Hectare.Terrain
import Hectare.Util (bundle)

import qualified Data.ByteString.Lazy as LB

import Control.Monad (replicateM, replicateM_, when)
import Data.Word

import Data.Bits
import Data.List (transpose)

import qualified Data.Map as M

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Foreign (mallocBytes, pokeElemOff)

import Graphics.Rendering.OpenGL ( PixelData(..), PixelFormat(..), TextureSize2D(..), 
                                   DataType(..) )

data ImageSGI = ImageSGI {
  storage   :: Word8,
  bpc       :: Word8,
  dimension :: Word16,
  xsize     :: Word16,
  ysize     :: Word16,
  zsize     :: Word16,
  pixmin    :: Word32,
  pixmax    :: Word32,
  imagename :: LB.ByteString,
  offsets   :: ([Word32], [Word32]),
  dataSGI   :: LB.ByteString
  }

emptyImageSGI :: ImageSGI
emptyImageSGI = ImageSGI 0 0 0 0 0 0 0 0 LB.empty ([],[]) LB.empty

instance Show ImageSGI where
  show image = "SGI " ++ 
               show (xsize image) ++ "x" ++ 
               show (ysize image) ++ "x" ++
               show (zsize image)

instance Binary ImageSGI where
  get = do
    magic      <- getWord16be
    when (magic /= 474) $ fail "Invalid image: incorrect magic number"
    storage'   <- getWord8
    bpc'       <- getWord8
    dimension' <- getWord16be
    xsize'     <- getWord16be
    ysize'     <- getWord16be
    zsize'     <- getWord16be
    pixmin'    <- getWord32be
    pixmax'    <- getWord32be
    skip 4
    imagename' <- getLazyByteString 80
    getWord32be --colormap
    skip 404
    offsets' <- case storage' of
      0 -> return ([],[])
      1 -> getOffsetTable (fromIntegral $ ysize' * zsize')
      _ -> fail $ "Invalid image: storage out of bounds" ++ show storage'
    dataSGI' <- getRemainingLazyByteString
    return $ ImageSGI storage' bpc' dimension' 
      xsize' ysize' zsize' pixmin' pixmax' 
      imagename' offsets' dataSGI'
  put image = do
    putWord16be 474 
    putWord8    $ storage image
    putWord8    $ bpc image
    putWord16be $ dimension image
    putWord16be $ xsize image
    putWord16be $ ysize image
    putWord16be $ zsize image
    putWord32be $ pixmin image
    putWord32be $ pixmax image
    replicateM_ 4 $ putWord8 0
    putLazyByteString $ imagename image
    putWord32be 0
    replicateM_ 404 $ putWord8 0
    when (storage image == 1) $
      putOffsetTable $ offsets image
    putLazyByteString $ dataSGI image

getOffsetTable :: Int -> Get ([Word32],[Word32])
getOffsetTable tablen = do
  startTab  <- replicateM tablen getWord32be
  lengthTab <- replicateM tablen getWord32be
  return (startTab, lengthTab)

putOffsetTable :: ([Word32],[Word32]) -> Put 
putOffsetTable (startTab, lengthTab) = do
  mapM_ putWord32be startTab
  mapM_ putWord32be lengthTab

instance ImageTexture ImageSGI where
  imagePixelData image = 
    case channels of
      Left  channels8  -> do 
        let rgbs = concat $ transpose channels8
        bytes <- mallocBytes (3*x*y)
        mapM_ (uncurry $ pokeElemOff bytes) $ zip [0..(3*x*y)-1] rgbs
        return (PixelData RGB UnsignedByte bytes)
      Right _ -> error "Cannot convert 16 bit textures."
    where
      x = fromIntegral $ xsize image
      y = fromIntegral $ ysize image
      channels = channelsSGI image
  imageSize image = 
    TextureSize2D (fromIntegral (xsize image)) (fromIntegral (ysize image))

channelsSGI :: ImageSGI -> Either [[Word8]] [[Word16]]
channelsSGI image =
  case storage image of
    0 -> case bpc image of
      1 -> Left  $ runGet (replicateM z (replicateM (x*y) getWord8)) (dataSGI image)
      2 -> Right $ runGet (replicateM z (replicateM (x*y) getWord16be)) (dataSGI image)
      _ -> error $ "Invalid image: bpc out of bounds" ++ show (bpc image)
    1 -> case bpc image of 
      1 -> Left  $ bundle (x*y) $ concatMap 
             (\offset -> runGet (getRowN getWord8)
               (LB.drop (fromIntegral offset - 512 - fromIntegral (y*z*8)) $ dataSGI image))
             starttab 
      2 -> Right $ bundle (x*y) $ concatMap
             (\offset -> runGet (getRowN getWord16be)
               (LB.drop (fromIntegral offset - 512 - fromIntegral (y*z*8)) $ dataSGI image))
             starttab 
      _ -> error $ "Invalid image: bpc out of bounds: " ++ show (bpc image)
    _ -> error $ "Invalid image: storage out of bounds" ++ show (storage image)
  where 
    x = fromIntegral $ xsize image
    y = fromIntegral $ ysize image
    z = fromIntegral $ zsize image
    (starttab, _) = offsets image

getRowN :: (Bits a, Integral a) => Get a -> Get [a]
getRowN getWordN = do
  pixel <- getWordN
  let stretch = testBit pixel 7
  let streak  = fromIntegral $ clearBit pixel 7
  case streak of 
    0 -> return []
    _ -> if stretch then
           do bytes    <- replicateM streak getWordN
              nextrows <- getRowN getWordN
              return (bytes ++ nextrows)
         else 
           do byte     <- getWordN
              nextrows <- getRowN getWordN
              return (replicate streak byte ++ nextrows)

instance ImageTerrain ImageSGI where
  imageTerrain image = 
    Terrain (xsize image) (ysize image) $ M.fromList [((fromIntegral x,fromIntegral y), (firstchannel !! fromIntegral ((y * xsize image) + x)) / (4*256)) | x <- [0.. fromIntegral (xsize image) - 1], y <- [0.. fromIntegral (ysize image) - 1]]
    where
      channels = either 
        ((map . map) fromIntegral) 
        ((map . map) fromIntegral) 
        (channelsSGI image)
      firstchannel = head channels

-- getRowN :: (Bits a, Integral a) => Get a -> Get [a]
-- getRowN getWordN = do
--   pixel <- getWordN
--   let stretch = {-# SCC "stretch" #-} testBit pixel 7
--   let streak  = {-# SCC "streak" #-}  fromIntegral $ clearBit pixel 7
--   case streak of 
--     0 -> {-# SCC "return-empty" #-} return []
--     _ -> if stretch then
--            do bytes    <- {-# SCC "bytes" #-}          replicateM streak getWordN
--               nextrows <- {-# SCC "nextrows-bytes" #-} getRowN getWordN
--               {-# SCC "return-bytes" #-} return (bytes ++ nextrows)
--          else 
--            do byte     <- {-# SCC "byte" #-}          getWordN
--               nextrows <- {-# SCC "nextrows-byte" #-} getRowN getWordN
--               {-# SCC "return-byte" #-} return (replicate streak byte ++ nextrows)
