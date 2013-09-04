{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types      #-}

module HGB.CPU where

import           Data.Word (Word8(..), Word16(..), Word(..))
import           Control.Lens
import           Control.Monad.State
import           Control.Applicative
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
dispatch 0x00 = trace "NOP"      $ iNOP
dispatch 0x01 = trace "LDSPd16"  $ iLDld16 (registers . lRR b c)

dispatch 0x11 = trace "LDSPd16"  $ iLDld16 (registers . lRR d e)

dispatch 0x21 = trace "LDSPd16"  $ iLDld16 (registers . lRR h l)

dispatch 0x31 = trace "LDSPd16"  $ iLDld16 (registers . lSP)

dispatch 0x40 = trace "LDrr_bb"  $ iLD b b
dispatch 0x41 = trace "LDrr_bc"  $ iLD b c
dispatch 0x42 = trace "LDrr_bd"  $ iLD b d
dispatch 0x43 = trace "LDrr_be"  $ iLD b e
dispatch 0x44 = trace "LDrr_bh"  $ iLD b h
dispatch 0x45 = trace "LDrr_bl"  $ iLD b l
dispatch 0x46 = trace "LDrHLm_b" $ iLDHL b lHLm
dispatch 0x47 = trace "LDrr_ba"  $ iLD b a
dispatch 0x48 = trace "LDrr_cb"  $ iLD c b
dispatch 0x49 = trace "LDrr_cc"  $ iLD c c
dispatch 0x4A = trace "LDrr_cd"  $ iLD c d
dispatch 0x4B = trace "LDrr_ce"  $ iLD c e
dispatch 0x4C = trace "LDrr_ch"  $ iLD c h
dispatch 0x4D = trace "LDrr_cl"  $ iLD c l
dispatch 0x4E = trace "LDrHLm_c" $ iLDHL c lHLm
dispatch 0x4F = trace "LDrr_ca"  $ iLD c a

dispatch 0x50 = trace "LDrr_db"  $ iLD d b
dispatch 0x51 = trace "LDrr_dc"  $ iLD d c
dispatch 0x52 = trace "LDrr_dd"  $ iLD d d
dispatch 0x53 = trace "LDrr_de"  $ iLD d e
dispatch 0x54 = trace "LDrr_dh"  $ iLD d h
dispatch 0x55 = trace "LDrr_dl"  $ iLD d l
dispatch 0x56 = trace "LDrHLm_d" $ iLDHL d lHLm
dispatch 0x57 = trace "LDrr_da"  $ iLD d a
dispatch 0x58 = trace "LDrr_eb"  $ iLD e b
dispatch 0x59 = trace "LDrr_ec"  $ iLD e c
dispatch 0x5A = trace "LDrr_ed"  $ iLD e d
dispatch 0x5B = trace "LDrr_ee"  $ iLD e e
dispatch 0x5C = trace "LDrr_eh"  $ iLD e h
dispatch 0x5D = trace "LDrr_el"  $ iLD e l
dispatch 0x5E = trace "LDrHLm_e" $ iLDHL e lHLm
dispatch 0x5F = trace "LDrr_ea"  $ iLD e a

dispatch 0x60 = trace "LDrr_hb"  $ iLD h b
dispatch 0x61 = trace "LDrr_hc"  $ iLD h c
dispatch 0x62 = trace "LDrr_hd"  $ iLD h d
dispatch 0x63 = trace "LDrr_he"  $ iLD h e
dispatch 0x64 = trace "LDrr_hh"  $ iLD h h
dispatch 0x65 = trace "LDrr_hl"  $ iLD h l
dispatch 0x66 = trace "LDrHLm_h" $ iLDHL h lHLm
dispatch 0x67 = trace "LDrr_ha"  $ iLD h a
dispatch 0x68 = trace "LDrr_lb"  $ iLD l b
dispatch 0x69 = trace "LDrr_lc"  $ iLD l c
dispatch 0x6A = trace "LDrr_ld"  $ iLD l d
dispatch 0x6B = trace "LDrr_le"  $ iLD l e
dispatch 0x6C = trace "LDrr_lh"  $ iLD l h
dispatch 0x6D = trace "LDrr_ll"  $ iLD l l
dispatch 0x6E = trace "LDrHLm_l" $ iLDHL l lHLm
dispatch 0x6F = trace "LDrr_la"  $ iLD l a

dispatch 0x70 = trace "LDHLmr_b" $ iLDHL lHLm b
dispatch 0x71 = trace "LDHLmr_c" $ iLDHL lHLm c
dispatch 0x72 = trace "LDHLmr_d" $ iLDHL lHLm d
dispatch 0x73 = trace "LDHLmr_e" $ iLDHL lHLm e
dispatch 0x74 = trace "LDHLmr_h" $ iLDHL lHLm h
dispatch 0x75 = trace "LDHLmr_l" $ iLDHL lHLm l
dispatch 0x76 = trace "halt"     $ undefined
dispatch 0x77 = trace "LDHLmr_c" $ iLDHL lHLm a

dispatch 0x78 = trace "LDrr_lb"  $ iLD a b
dispatch 0x79 = trace "LDrr_lc"  $ iLD a c
dispatch 0x7A = trace "LDrr_ld"  $ iLD a d
dispatch 0x7B = trace "LDrr_le"  $ iLD a e
dispatch 0x7C = trace "LDrr_lh"  $ iLD a h
dispatch 0x7D = trace "LDrr_ll"  $ iLD a l
dispatch 0x7E = trace "LDrHLm_a" $ iLDHL a lHLm
dispatch 0x7F = trace "LDrr_aa"  $ iLD a a

dispatch 0xA8 = trace "XORr_b"   $ iXOR b
dispatch 0xA9 = trace "XORr_c"   $ iXOR c
dispatch 0xAA = trace "XORr_d"   $ iXOR d
dispatch 0xAB = trace "XORr_e"   $ iXOR e
dispatch 0xAC = trace "XORr_h"   $ iXOR h
dispatch 0xAD = trace "XORr_l"   $ iXOR l
dispatch 0xAE = trace "XORHLm"   $ iXORHL lHLm
dispatch 0xAF = trace "XORr_a"   $ iXOR a

dispatch 0xB8 = trace "CPr_b" $ iCPr_b >> mkClock 1 4
dispatch op   = error $ "Instruction not implemented : " ++ (printf "0x%02x" op)

-- | Reset the flags to 0
fReset :: VmS ()
fReset = f .= 0

-- | Set underflow flag if true
fUnderflow :: Bool -> VmS ()
fUnderflow b = do
  case b of
    True  -> f .|.= fC
    False -> return ()
-- | Set the zero flag if null
fZero :: Word8 -> VmS ()
fZero w = do
  case w of
    0 -> f .|.= fZ >> return ()
    _ -> return ()

-- | Set the halfcarry flag if true
fHalfcarry :: Bool -> VmS ()
fHalfcarry b = do
  case b of
    True  -> f .|.= fH
    False -> return ()

-- | Create a Clock in the VmS monad
mkClock :: Word -> Word -> VmS Clock
mkClock mV tV = return $ Clock mV tV

-- | No OPeration
iNOP = mkClock 1 4

-- | LD instruction that can be used with any Lens' Vm b
--   Syntax : `LD Dst Src`
--
--   > LD Register <- Register
iLD :: ASetter' Registers b -> Getting b Registers b -> VmS Clock
iLD output input = registers . output <~ use (registers . input) >> mkClock 1 4


-- | Same as LD, but alow to use (HL) on the right / left
--   Syntax : `LD Dst Src`
--
--   > LD (Register | (HL) ) <- (Register | HL)
iLDHL :: ASetter' Vm b -> Getting b Vm b -> VmS Clock
iLDHL output input =  output <~ use (input) >> mkClock 1 8

-- | LD (SP | HL | DE | BC) <- immediate Word16
iLDld16 :: ASetter' Vm Word16 -> VmS Clock
iLDld16 output = output <~ readProgramW >> (mkClock 3 12)

-- | XOR the register 'a' with a register 'R' into 'a'
--   Syntax : `XOR Src`
iXOR :: Getting Word8 Registers Word8 -> VmS Clock
iXOR input = iXORHL (registers . input) >> mkClock 1 4

-- | XOR the register a with (register R | (HL)) into a
--  Syntax : `XOR Src`
iXORHL :: Getting Word8 Vm Word8 -> VmS Clock
iXORHL input = do
  fReset
  (a %=) . xor =<< use input
  mkClock 1 8

-- | Read from (RR) : The value at location r:r
readRRm :: Getting Word8 Registers Word8 -> Getting Word8 Registers Word8 -> Vm -> Word8
readRRm h' l' vm = rb idx (vm ^. mmu)
  where idx = readRR h' l' (vm ^. registers)

-- | Write on (RR) : Write at location r:r
writeRRm :: Getting Word8 Registers Word8 -> Getting Word8 Registers Word8 -> Vm -> Word8 -> Vm
writeRRm h' l' vm v = mmu %~ (wb idx v) $ vm
  where idx = readRR h' l' (vm ^. registers)

-- | (HL) saw as a lens
lHLm :: Lens' Vm Word8
lHLm f vm = (writeRRm h l vm) <$> f (readRRm h l vm)

-- | RR (where R means a register) saw as a lens
lRR :: ALens' Registers Word8 -> ALens' Registers Word8 -> Lens' Registers Word16
lRR h' l' f reg = writeRR (cloneLens h') (cloneLens l') reg <$>
                  f (readRR (cloneLens h') (cloneLens l') reg)

-- | SP saw as a lens
lSP :: Lens' Registers Word16
lSP f reg = (flip `fmap` set) sp reg <$> f (reg ^. sp)

-- | Compute r:r as a 16 bits addr
readRR :: Getting Word8 Registers Word8 -> Getting Word8 Registers Word8 -> Registers ->  Word16
readRR h' l' reg = wCombine (reg ^. h') (reg ^. l')

-- | Compute r:r as a 16 bits addr
writeRR :: ASetter' Registers Word8 -> ASetter' Registers Word8 -> Registers -> Word16 -> Registers
writeRR h' l' reg v = h' .~ (fromIntegral $ shiftR v 8) $
                      l' .~ (fromIntegral v) $ reg

-- | Compute h:l as a 16 bits addr
readDE :: Registers ->  Word16
readDE reg = wCombine (reg ^. d) (reg ^. l)

-- | Compute h:l as a 16 bits addr
writeDE :: Registers -> Word16 -> Registers
writeDE reg v = reg {_l = fromIntegral $ shiftR v 8, _h = fromIntegral v}


-- | Compare B to A and set the flags
iCPr_b :: VmS ()
iCPr_b = do
  a' <- use $ a
  b' <- use $ b
  let diff = a' - b'
  fReset
  fUnderflow $ a' < b'
  fZero diff
  fHalfcarry True
