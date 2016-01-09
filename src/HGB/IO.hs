{- | IO Module (Contain all the IO access at FF00-FF7F

Export functions reading / writing to the GPU registers,
joypad flags...

Called by the MMU module.

-}
module HGB.IO where

import           Data.Word (Word8, Word16)
import           HGB.Types
import           Control.Lens


dispatchIOread :: Word16 -> Mmu -> Word8
dispatchIOread addr mmu'
  | addr == 0xFF40 = lcdcToWord8 $ mmu' ^. lcdcf
  | addr == 0xFF42 = mmu' ^. scy
  | addr == 0xFF43 = mmu' ^. scx
  | addr == 0xFF44 = fromIntegral $ mmu' ^. gpuLine
  | otherwise = 0 -- TODO

dispatchIOwrite :: Word16 -> Word8 -> Mmu -> Mmu
dispatchIOwrite addr value mmu'
  | addr == 0xFF40 = lcdcf     .~ word8ToLCDC value $ mmu'
  | addr == 0xFF42 = scy       .~ value $ mmu'
  | addr == 0xFF43 = scx       .~ value $ mmu'
  | addr == 0xFF47 = bgPalette .~ value $ mmu'
  | otherwise = mmu' -- TODO
