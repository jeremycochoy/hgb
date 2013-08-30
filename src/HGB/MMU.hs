module HGB.MMU
  ( rb
  , rw
  ) where

import           Data.Word (Word8(..), Word16(..), Word(..))
import           HGB.Types

-- | Read a byte from MMU (TODO)
rb :: Mmu -> Word8
rb _ = 0

-- | Read a word from MMU (TODO)
rw :: Mmu -> Word16
rw _ = 0

-- | Write a byte from MMU (TODO)
wb :: Word8 -> Mmu -> Mmu
wb _ x = x

-- | Write a word from MMU (TODO)
ww :: Word8 -> Mmu -> Mmu
ww _ x = x
