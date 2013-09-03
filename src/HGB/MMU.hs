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

import           Data.Word (Word8(..), Word16(..), Word(..))
import           HGB.Types
import           Data.Vector.Unboxed ((!))
import           Control.Lens

-- | Read a byte from MMU (TODO)
rb :: Word16 -> Mmu -> Word8
rb addr m = (m ^. bios) ! (fromIntegral addr)

-- | Read a word from MMU (TODO)
rw :: Word16 -> Mmu -> Word16
rw _ _ = 0

-- | Write a byte from MMU (TODO)
wb :: Word16 -> Word8 -> Mmu -> Mmu
wb _ _ x = x

-- | Write a word from MMU (TODO)
ww :: Word16 -> Word8 -> Mmu -> Mmu
ww _ _ x = x
