{-# LANGUAGE TemplateHaskell #-}

module HGB.GPU where

import           Data.Word (Word8, Word16, Word)
import           Control.Lens
import           Control.Monad.State
import           HGB.Types
import           HGB.MMU
import           HGB.Lens

updateGPUmode :: VmS ()
updateGPUmode = do
  return ()
