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
