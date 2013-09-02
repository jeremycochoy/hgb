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

-- | Read the byte pointed by PC and increment it
readProgramByte :: VmS Word8
readProgramByte = do
  pc' <- use $ pc
  byte <- (rb pc') `liftM` (use mmu)
  pc += 1
  return byte

-- | Execute exactly one instruction
exec :: VmS ()
exec = do
  inst <- dispatch `liftM` readProgramByte

  inst ^. iVmS

  t += inst ^. clock.t
  m += inst ^. clock.m

  return ()

dispatch :: Word8 -> Instruction
dispatch 0x00 = trace "nop" $ Instruction (Clock 1 4) iNOP
dispatch 0x40 = trace "LDrr_bb" $ Instruction (Clock 1 4) iLDrr_bb
dispatch 0xB8 = trace "CPr_b" $ Instruction (Clock 1 4) iCPr_b
dispatch op   = error $ "Instruction not implemented : " ++ (show op)

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

-- | No OPeration
iNOP = return ()

-- LD Dst Src

-- Those are LD Register <- Register operations

iLDrr_aa :: VmS ()
iLDrr_aa = a <~ use a
iLDrr_ab :: VmS ()
iLDrr_ab = a <~ use b
iLDrr_ac :: VmS ()
iLDrr_ac = a <~ use c
iLDrr_ad :: VmS ()
iLDrr_ad = a <~ use d
iLDrr_ae :: VmS ()
iLDrr_ae = a <~ use e
iLDrr_ah :: VmS ()
iLDrr_ah = a <~ use h
iLDrr_al :: VmS ()
iLDrr_al = a <~ use l

iLDrr_ba :: VmS ()
iLDrr_ba = b <~ use a
iLDrr_bb :: VmS ()
iLDrr_bb = b <~ use b
iLDrr_bc :: VmS ()
iLDrr_bc = b <~ use c
iLDrr_bd :: VmS ()
iLDrr_bd = b <~ use d
iLDrr_be :: VmS ()
iLDrr_be = b <~ use e
iLDrr_bh :: VmS ()
iLDrr_bh = b <~ use h
iLDrr_bl :: VmS ()
iLDrr_bl = b <~ use l

iLDrr_ca :: VmS ()
iLDrr_ca = c <~ use a
iLDrr_cb :: VmS ()
iLDrr_cb = c <~ use b
iLDrr_cc :: VmS ()
iLDrr_cc = c <~ use c
iLDrr_cd :: VmS ()
iLDrr_cd = c <~ use d
iLDrr_ce :: VmS ()
iLDrr_ce = c <~ use e
iLDrr_ch :: VmS ()
iLDrr_ch = c <~ use h
iLDrr_cl :: VmS ()
iLDrr_cl = c <~ use l

iLDrr_da :: VmS ()
iLDrr_da = d <~ use a
iLDrr_db :: VmS ()
iLDrr_db = d <~ use b
iLDrr_dc :: VmS ()
iLDrr_dc = d <~ use c
iLDrr_dd :: VmS ()
iLDrr_dd = d <~ use d
iLDrr_de :: VmS ()
iLDrr_de = d <~ use e
iLDrr_dh :: VmS ()
iLDrr_dh = d <~ use h
iLDrr_dl :: VmS ()
iLDrr_dl = d <~ use l

iLDrr_ea :: VmS ()
iLDrr_ea = e <~ use a
iLDrr_eb :: VmS ()
iLDrr_eb = e <~ use b
iLDrr_ec :: VmS ()
iLDrr_ec = e <~ use c
iLDrr_ed :: VmS ()
iLDrr_ed = e <~ use d
iLDrr_ee :: VmS ()
iLDrr_ee = e <~ use e
iLDrr_eh :: VmS ()
iLDrr_eh = e <~ use h
iLDrr_el :: VmS ()
iLDrr_el = e <~ use l

iLDrr_ha :: VmS ()
iLDrr_ha = h <~ use a
iLDrr_hb :: VmS ()
iLDrr_hb = h <~ use b
iLDrr_hc :: VmS ()
iLDrr_hc = h <~ use c
iLDrr_hd :: VmS ()
iLDrr_hd = h <~ use d
iLDrr_he :: VmS ()
iLDrr_he = h <~ use e
iLDrr_hh :: VmS ()
iLDrr_hh = h <~ use h
iLDrr_hl :: VmS ()
iLDrr_hl = h <~ use l

iLDrr_la :: VmS ()
iLDrr_la = h <~ use a
iLDrr_lb :: VmS ()
iLDrr_lb = h <~ use b
iLDrr_lc :: VmS ()
iLDrr_lc = h <~ use c
iLDrr_ld :: VmS ()
iLDrr_ld = h <~ use d
iLDrr_le :: VmS ()
iLDrr_le = h <~ use e
iLDrr_lh :: VmS ()
iLDrr_lh = h <~ use h
iLDrr_ll :: VmS ()
iLDrr_ll = h <~ use l

-- Those are LD Register <- MMU operations

readHLm :: VmS Word8
readHLm = do
  h' <- use h
  l' <- use l
  -- Compute a 16 bits addr as h:l
  let idx = ((shiftL (fromIntegral h') 8) + (fromIntegral l')) :: Word16
  (rb idx) `liftM` (use mmu)

iLDrHLm_a :: VmS ()
iLDrHLm_a = a <~ readHLm
iLDrHLm_b :: VmS ()
iLDrHLm_b = b <~ readHLm
iLDrHLm_c :: VmS ()
iLDrHLm_c = c <~ readHLm
iLDrHLm_d :: VmS ()
iLDrHLm_d = d <~ readHLm
iLDrHLm_e :: VmS ()
iLDrHLm_e = e <~ readHLm
iLDrHLm_h :: VmS ()
iLDrHLm_h = h <~ readHLm
iLDrHLm_l :: VmS ()
iLDrHLm_l = l <~ readHLm


writeHLm :: Word8 -> VmS ()
writeHLm v = do
  h' <- use h
  l' <- use l
  -- Compute a 16 bits addr as h:l
  let idx = ((shiftL (fromIntegral h') 8) + (fromIntegral l')) :: Word16
  mmu %= (wb idx v)


-- | Compare B to A and set the flags
iCPr_b :: VmS ()
iCPr_b = do
  a' <- use $ a
  b' <- use $ b
  let diff = a' - b'
  underflow $ a' < b'
  zero diff
  halfcarry True
