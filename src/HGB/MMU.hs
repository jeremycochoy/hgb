{- | MMU Module (Contain all the non-graphic memory)

> From: Pan Docs - nocash / kOOPa
>
> General Memory Map
>
>  0000-3FFF   16KB ROM Bank 00     (in cartridge, fixed at bank 00)
>  4000-7FFF   16KB ROM Bank 01..NN (in cartridge, switchable bank number)
>  8000-9FFF   8KB Video RAM (VRAM) (switchable bank 0-1 in CGB Mode)
>  A000-BFFF   8KB External RAM     (in cartridge, switchable bank, if any)
>  C000-CFFF   4KB Work RAM Bank 0 (WRAM)
>  D000-DFFF   4KB Work RAM Bank 1 (WRAM)  (switchable bank 1-7 in CGB Mode)
>  E000-FDFF   Same as C000-DDFF (ECHO)    (typically not used)
>  FE00-FE9F   Sprite Attribute Table (OAM)
>  FEA0-FEFF   Not Usable
>  FF00-FF7F   I/O Ports
>  FF80-FFFE   High RAM (HRAM)
>  FFFF        Interrupt Enable Register
-}
module HGB.MMU
  ( rb
  , rw
  , wb
  , ww
  ) where

import           Data.Word (Word8, Word16)
import           HGB.Types
import           Data.Vector.Unboxed ((!), (//))
import           Control.Lens
import qualified Data.Vector.Unboxed

(!&) :: Integral a => Data.Vector.Unboxed.Vector Word8 -> a -> Word8
(!&) a b = a ! (fromIntegral b)
infixr 3 !&

-- | Read a byte from MMU (TODO)
rb :: Word16 -> Mmu -> Word8
rb addr mmu'
  | addr < 0x100 = case (mmu' ^. biosEnabled) of
    True  -> mmu' ^. bios !& addr
    False -> mmu' ^. rom  !& addr
  | addr < 0x4000 = mmu' ^. rom   !& addr
  | addr < 0x8000 = mmu' ^. srom  !& (addr - 0x4000)
  | addr < 0xA000 = error "VRam not implemented..."
  | addr < 0xC000 = mmu' ^. eram  !& (addr - 0xA000)
  | addr < 0xD000 = mmu' ^. wram  !& (addr - 0xC000)
  | addr < 0xE000 = mmu' ^. swram !& (addr - 0xD000)
  | addr < 0xFE00 = mmu' ^. wram  !& (addr - 0xE000)
  | addr == 0xFFFF = mmu' ^. ier
  | otherwise = error $ "rb" ++ show addr ++ "Not Implemented..."

-- | Read a word from MMU
rw :: Word16 -> Mmu -> Word16
rw addr mmu' = wCombine (flip rb mmu' $ addr + 1) (rb addr mmu')

-- | Write a byte from MMU (TODO)
wb :: Word16 -> Word8 -> Mmu -> Mmu
wb addr value mmu'
  | addr < 0x8000 = mmu' -- ROM is Read Only
  | addr < 0xA000 = error $ "VRAM not implemented"
  | addr < 0xC000 = eram  %~ up (addr - 0xC000) $ mmu'
  | addr < 0xD000 = wram  %~ up (addr - 0xD000) $ mmu'
  | addr < 0xE000 = swram %~ up (addr - 0xE000) $ mmu'
  | addr == 0xFFFF = ier .~ value $ mmu'
  where
    up addr vec = vec // [(fromIntegral addr, value)]

-- | Write a word from MMU
ww :: Word16 -> Word16 -> Mmu -> Mmu
ww addr value mmu' = wb (addr + 1) h' . wb addr l' $ mmu'
  where
    (h', l') = wUncombine value
