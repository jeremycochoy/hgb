{-# LANGUAGE TemplateHaskell #-}

module HGB.GPU where

import           Data.Word (Word8, Word16)
import           Data.Int (Int8, Int16)
import           Control.Lens
import           Control.Monad.State
import           HGB.Types
import           HGB.MMU
import           HGB.Lens
import           Data.List (unfoldr)
import           Data.Bits hiding (bit)


-- | Read a word from the tile map starting at 'addr'
readTileMapI :: Word16 -> Word16 -> Word16 -> Mmu -> Word8
readTileMapI addr x y mmu' = rb (addr + x + y * 256) mmu'

-- | Read a word from the tile map 0
readTileMap0 :: Word16 -> Word16 -> Mmu -> Word8
readTileMap0 = readTileMapI 0x9800

-- | Read a word from the tile map 1
readTileMap1 :: Word16 -> Word16 -> Mmu -> Word8
readTileMap1 = readTileMapI 0x9C00

-- | Read the line 'line' from tile 'tileID'
--   from the tile set located at 'addr'
--   as a list of Word16 pixels
readTileLineI :: Word16 -> Int16 -> Word8 -> Mmu -> Word16
readTileLineI addr tileID line mmu' = rw lineAddr mmu'
  where
    tileAddr = addr + (fromIntegral tileID) * 16
    lineAddr = tileAddr + (fromIntegral line) * 2

-- | Read a tile from tile set 0 at location loc
readTileLine0 :: Word8 -> Word8 -> Mmu -> Word16
readTileLine0 tileID = readTileLineI 0x8000 (fromIntegral tileID)
readTileLine1 :: Word8 -> Word8 -> Mmu -> Word16
readTileLine1 tileID = readTileLineI 0x9000 (fromIntegral signedTileID)
  where
    signedTileID = (fromIntegral tileID) :: Int8

-- | Take tile line, and produce a list of 8 pixels.
--   each pixel is of one of the four colors in the 'GreyScale'.
--
--   The input is two interleaved bytes l(ow) an h(ight), storing the color
--   of a line of 8 pixels where each color is stored in two
--   bits. The hight bit is stored in the second byte (namely h)
--   and the lower one in l(low).
--
--   Below see an example of a line of pixels
--
--  > 0x803E : 01001110 = 0x4E <- the l byte
--  > 0x803F : 10001011 = 0x8B <- the h byte
--  >
--  >          21003132   <- The colors in [0=WHITE, 3=BLACK]
colorFromTileRow :: Word16 -> [GreyScale]
colorFromTileRow pixels = unfoldr readColor (0, l, h)
  where
    (l, h) = wUncombine pixels
    readColor (idx, l, h) = case idx of
      8 -> Nothing
      _ -> Just (color, (idx + 1, l', h'))
        where
          l' = l `shiftL` 1
          h' = h `shiftL` 1
          color = numToGS $ (h `shiftR` 7) * 2 + (l `shiftR` 7)

gpuModeBinary :: GpuMode -> Word8
gpuModeBinary HorizontalBlank = 0
gpuModeBinary ScanlineOAM     = 2
gpuModeBinary ScanlineVRAM    = 3
gpuModeBinary VerticalBlank   = 1

updateGPUmode :: Word -> VmS ()
updateGPUmode t = do
  -- Update the clock value
  gpuClock += t
  -- Select wich change should happen
  gpuClock' <- use $ gpuClock
  gpuMode'  <- use $ gpuMode
  case gpuMode' of
    HorizontalBlank | gpuClock' >= 204 -> do
      gpuClock -= 204
      gpuLine' <- gpuLine <+= 1
    -- If it's the last line of the screen
      if gpuLine' == 143
        then gpuMode .= VerticalBlank
        else gpuMode .= ScanlineOAM
      renderLine
    ScanlineOAM | gpuClock' >= 80 -> do
      gpuClock -= 80
      gpuMode .= ScanlineVRAM
    ScanlineVRAM | gpuClock' >= 172 -> do
      gpuClock -= 172
      gpuMode .= HorizontalBlank
      -- TODO: Should do something to render the line
    VerticalBlank | gpuClock' >= 456 -> do
      gpuClock -= 456
      gpuLine' <- gpuLine <+= 1
      -- After 10 "lines" go back to scanline
      when (gpuLine' == 153) $ do
        gpuLine .= 0
        gpuMode .= ScanlineOAM
    _ -> return ()


renderLine :: VmS ()
renderLine = do
  return ()

