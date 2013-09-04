{-# LANGUAGE TemplateHaskell #-}

module HGB.CPU where

import           Data.Word (Word8(..), Word16(..), Word(..))
import           Control.Lens
import           Control.Monad.State
import           Data.Default
import           Data.Bits
import           Data.Bits.Lens
import           HGB.Types
import           HGB.MMU
import           Debug.Trace
import           Text.Printf (printf)

-- | Read the byte pointed by PC and increment it
readProgramB :: VmS Word8
readProgramB = do
  pc' <- use $ pc
  pc += 1
  (rb pc') `liftM` (use mmu)

-- | Read the byte pointed by PC and increment it
readProgramW :: VmS Word16
readProgramW = do
  pc' <- use $ pc
  pc += 2
  (rw pc') `liftM` (use mmu)

-- | Execute exactly one instruction
exec :: VmS ()
exec = do
  inst <- dispatch =<< readProgramB

  t += inst ^. t
  m += inst ^. m

  return ()

dispatch :: Word8 -> Instruction
dispatch 0x00 = trace "nop"      $ iNOP

dispatch 0x31 = trace "LDSPd16"  $ iLDSPd16

dispatch 0x40 = trace "LDrr_bb"  $ iLDrr b b
dispatch 0x41 = trace "LDrr_bc"  $ iLDrr b c
dispatch 0x42 = trace "LDrr_bd"  $ iLDrr b d
dispatch 0x43 = trace "LDrr_be"  $ iLDrr b e
dispatch 0x44 = trace "LDrr_bh"  $ iLDrr b h
dispatch 0x45 = trace "LDrr_bl"  $ iLDrr b l
dispatch 0x46 = trace "LDrHLm_b" $ iLDrHLm b
dispatch 0x47 = trace "LDrr_ba"  $ iLDrr b a
dispatch 0x48 = trace "LDrr_cb"  $ iLDrr c b
dispatch 0x49 = trace "LDrr_cc"  $ iLDrr c c
dispatch 0x4A = trace "LDrr_cd"  $ iLDrr c d
dispatch 0x4B = trace "LDrr_ce"  $ iLDrr c e
dispatch 0x4C = trace "LDrr_ch"  $ iLDrr c h
dispatch 0x4D = trace "LDrr_cl"  $ iLDrr c l
dispatch 0x4E = trace "LDrHLm_c" $ iLDrHLm c
dispatch 0x4F = trace "LDrr_ca"  $ iLDrr c a

dispatch 0x50 = trace "LDrr_db"  $ iLDrr d b
dispatch 0x51 = trace "LDrr_dc"  $ iLDrr d c
dispatch 0x52 = trace "LDrr_dd"  $ iLDrr d d
dispatch 0x53 = trace "LDrr_de"  $ iLDrr d e
dispatch 0x54 = trace "LDrr_dh"  $ iLDrr d h
dispatch 0x55 = trace "LDrr_dl"  $ iLDrr d l
dispatch 0x56 = trace "LDrHLm_d" $ iLDrHLm d
dispatch 0x57 = trace "LDrr_da"  $ iLDrr d a
dispatch 0x58 = trace "LDrr_eb"  $ iLDrr e b
dispatch 0x59 = trace "LDrr_ec"  $ iLDrr e c
dispatch 0x5A = trace "LDrr_ed"  $ iLDrr e d
dispatch 0x5B = trace "LDrr_ee"  $ iLDrr e e
dispatch 0x5C = trace "LDrr_eh"  $ iLDrr e h
dispatch 0x5D = trace "LDrr_el"  $ iLDrr e l
dispatch 0x5E = trace "LDrHLm_e" $ iLDrHLm e
dispatch 0x5F = trace "LDrr_ea"  $ iLDrr e a

dispatch 0x60 = trace "LDrr_hb"  $ iLDrr h b
dispatch 0x61 = trace "LDrr_hc"  $ iLDrr h c
dispatch 0x62 = trace "LDrr_hd"  $ iLDrr h d
dispatch 0x63 = trace "LDrr_he"  $ iLDrr h e
dispatch 0x64 = trace "LDrr_hh"  $ iLDrr h h
dispatch 0x65 = trace "LDrr_hl"  $ iLDrr h l
dispatch 0x66 = trace "LDrHLm_h" $ iLDrHLm h
dispatch 0x67 = trace "LDrr_ha"  $ iLDrr h a
dispatch 0x68 = trace "LDrr_lb"  $ iLDrr l b
dispatch 0x69 = trace "LDrr_lc"  $ iLDrr l c
dispatch 0x6A = trace "LDrr_ld"  $ iLDrr l d
dispatch 0x6B = trace "LDrr_le"  $ iLDrr l e
dispatch 0x6C = trace "LDrr_lh"  $ iLDrr l h
dispatch 0x6D = trace "LDrr_ll"  $ iLDrr l l
dispatch 0x6E = trace "LDrHLm_l" $ iLDrHLm l
dispatch 0x6F = trace "LDrr_la"  $ iLDrr l a

dispatch 0x70 = trace "LDHLmr_b" $ iLDHLmr b
dispatch 0x71 = trace "LDHLmr_c" $ iLDHLmr c
dispatch 0x72 = trace "LDHLmr_d" $ iLDHLmr d
dispatch 0x73 = trace "LDHLmr_e" $ iLDHLmr e
dispatch 0x74 = trace "LDHLmr_h" $ iLDHLmr h
dispatch 0x75 = trace "LDHLmr_l" $ iLDHLmr l
dispatch 0x76 = trace "halt"     $ undefined
dispatch 0x77 = trace "LDHLmr_c" $ iLDHLmr a

dispatch 0x78 = trace "LDrr_lb"  $ iLDrr a b
dispatch 0x79 = trace "LDrr_lc"  $ iLDrr a c
dispatch 0x7A = trace "LDrr_ld"  $ iLDrr a d
dispatch 0x7B = trace "LDrr_le"  $ iLDrr a e
dispatch 0x7C = trace "LDrr_lh"  $ iLDrr a h
dispatch 0x7D = trace "LDrr_ll"  $ iLDrr a l
dispatch 0x7E = trace "LDrHLm_a" $ iLDrHLm a
dispatch 0x7F = trace "LDrr_aa"  $ iLDrr a a

dispatch 0xB8 = trace "CPr_b" $ iCPr_b >> mkClock 1 4
dispatch op   = error $ "Instruction not implemented : " ++ (printf "0x%02x" op)

-- | Set underflow flag if true
underflow :: Bool -> VmS ()
underflow b = do
  -- Underflow
  case b of
    True  -> f .|.= fC
    False -> return ()
-- | Set the zero flag if null
zero :: Word8 -> VmS ()
zero w = do
  -- w is null
  case w of
    0 -> f .|.= fZ >> return ()
    _ -> return ()

-- | Set the halfcarry flag if true
halfcarry :: Bool -> VmS ()
halfcarry b = do
  case b of
    True  -> f .|.= fH
    False -> return ()

-- | A lot of instruction take 1 / 4 to complete.
mkClock :: Word -> Word -> VmS Clock
mkClock mV tV = return $ Clock mV tV

-- | No OPeration
iNOP = mkClock 1 4


-- LD Dst Src

-- | LD Register <- Register
iLDrr :: ASetter' Registers Word8 -> Getting Word8 Registers Word8 -> VmS Clock
iLDrr output input = registers . output <~ use (registers . input) >> mkClock 1 4

-- | LD Register <- MMU at H:L
iLDrHLm :: ASetter' Registers Word8 -> VmS Clock
iLDrHLm r = (registers . r <~ readHLm) >> (mkClock 1 8)

readHLm :: VmS Word8
readHLm = do
  idx <- getHLaddr
  (rb idx) `liftM` (use mmu)

-- | LD MMU at H:L <- Register
iLDHLmr :: Getting Word8 Registers Word8 -> VmS Clock
iLDHLmr r = use (registers . r) >>= writeHLm >> (mkClock 1 8)

writeHLm :: Word8 -> VmS ()
writeHLm v = do
  idx <- getHLaddr
  mmu %= (wb idx v)

-- | LD SP <- immediate Word16
iLDSPd16 :: VmS Clock
iLDSPd16 = sp <~ readProgramW >> (mkClock 3 12)

-- | Compute h:l as a 16 bits addr
getHLaddr :: VmS Word16
getHLaddr = do
  h' <- use h
  l' <- use l
  return $ wCombine h' l'

-- | Compare B to A and set the flags
iCPr_b :: VmS ()
iCPr_b = do
  a' <- use $ a
  b' <- use $ b
  let diff = a' - b'
  underflow $ a' < b'
  zero diff
  halfcarry True
