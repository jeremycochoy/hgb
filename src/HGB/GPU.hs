{-# LANGUAGE TemplateHaskell #-}

module HGB.GPU where

import           Data.Word (Word8, Word16, Word)
import           Control.Lens
import           Control.Monad.State
import           HGB.Types
import           HGB.MMU
import           HGB.Lens

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
