{-# LANGUAGE Rank2Types      #-}
module HGB.CPU where

import           Data.Word (Word8, Word16, Word)
import           Control.Lens
import           Control.Monad.State
import           Control.Applicative
import           HGB.Types
import           HGB.MMU
import           HGB.Lens
import           Data.Bits
--import           Debug.Trace
import           Text.Printf (printf)

trace :: String -> a -> a
trace _ x = x

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

-- | Select the right instruction from the opcode.
dispatch :: Word8 -> Instruction
dispatch 0x00 = trace "NOP"      $ iNOP
dispatch 0x01 = trace "LDSPd16"  $ iLDd16 lBC
dispatch 0x02 = trace "LDBCma"   $ iLDHL lBCm a
dispatch 0x06 = trace "LDbd8"    $ iLDd8 b
dispatch 0x0A = trace "LDaBCm"   $ iLDHL a lBCm
dispatch 0x0E = trace "LDcd8"    $ iLDd8 c

dispatch 0x11 = trace "LDSPd16"  $ iLDd16 lDE
dispatch 0x12 = trace "LDDEma"   $ iLDHL lDEm a
dispatch 0x16 = trace "LDdd8"    $ iLDd8 d
dispatch 0x18 = trace "JRr8"     $ iJR
dispatch 0x1A = trace "LDaDEm"   $ iLDHL a lDEm
dispatch 0x1E = trace "LDed8"    $ iLDd8 e

dispatch 0x20 = trace "JRNZr8"   $ iJRf lNZf
dispatch 0x21 = trace "LDSPd16"  $ iLDd16 lHL
dispatch 0x22 = trace "LDIHLma"  $ iLDI lHLm a
dispatch 0x26 = trace "LDhd8"    $ iLDd8 h
dispatch 0x28 = trace "JRZr8"    $ iJRf lZf
dispatch 0x2A = trace "LDIaHLm"  $ iLDI a lHLm
dispatch 0x2E = trace "LDld8"    $ iLDd8 l

dispatch 0x30 = trace "JRNCr8"   $ iJRf lNCf
dispatch 0x31 = trace "LDSPd16"  $ iLDd16 sp
dispatch 0x32 = trace "LDDHLma"  $ iLDD lHLm a
dispatch 0x36 = trace "LDHLmd8"  $ iLDHLd8 lHLm
dispatch 0x38 = trace "JRCr8"    $ iJRf lCf
dispatch 0x3A = trace "LDIaHLm"  $ iLDD a lHLm
dispatch 0x3E = trace "LDad8"    $ iLDd8 a

dispatch 0x40 = trace "LDbb"     $ iLD b b
dispatch 0x41 = trace "LDbc"     $ iLD b c
dispatch 0x42 = trace "LDbd"     $ iLD b d
dispatch 0x43 = trace "LDbe"     $ iLD b e
dispatch 0x44 = trace "LDbh"     $ iLD b h
dispatch 0x45 = trace "LDbl"     $ iLD b l
dispatch 0x46 = trace "LDbHLm"   $ iLDHL b lHLm
dispatch 0x47 = trace "LDba"     $ iLD b a
dispatch 0x48 = trace "LDcb"     $ iLD c b
dispatch 0x49 = trace "LDcc"     $ iLD c c
dispatch 0x4A = trace "LDcd"     $ iLD c d
dispatch 0x4B = trace "LDce"     $ iLD c e
dispatch 0x4C = trace "LDch"     $ iLD c h
dispatch 0x4D = trace "LDcl"     $ iLD c l
dispatch 0x4E = trace "LDcHLm"   $ iLDHL c lHLm
dispatch 0x4F = trace "LDca"     $ iLD c a

dispatch 0x50 = trace "LDdb"     $ iLD d b
dispatch 0x51 = trace "LDdc"     $ iLD d c
dispatch 0x52 = trace "LDdd"     $ iLD d d
dispatch 0x53 = trace "LDde"     $ iLD d e
dispatch 0x54 = trace "LDdh"     $ iLD d h
dispatch 0x55 = trace "LDdl"     $ iLD d l
dispatch 0x56 = trace "LDdHLm"  $ iLDHL d lHLm
dispatch 0x57 = trace "LDda"     $ iLD d a
dispatch 0x58 = trace "LDeb"     $ iLD e b
dispatch 0x59 = trace "LDec"     $ iLD e c
dispatch 0x5A = trace "LDed"     $ iLD e d
dispatch 0x5B = trace "LDee"     $ iLD e e
dispatch 0x5C = trace "LDeh"     $ iLD e h
dispatch 0x5D = trace "LDel"     $ iLD e l
dispatch 0x5E = trace "LDeHLm"   $ iLDHL e lHLm
dispatch 0x5F = trace "LDea"     $ iLD e a

dispatch 0x60 = trace "LDhb"     $ iLD h b
dispatch 0x61 = trace "LDhc"     $ iLD h c
dispatch 0x62 = trace "LDhd"     $ iLD h d
dispatch 0x63 = trace "LDhe"     $ iLD h e
dispatch 0x64 = trace "LDhh"     $ iLD h h
dispatch 0x65 = trace "LDhl"     $ iLD h l
dispatch 0x66 = trace "LDhHLm"   $ iLDHL h lHLm
dispatch 0x67 = trace "LDha"     $ iLD h a
dispatch 0x68 = trace "LDlb"     $ iLD l b
dispatch 0x69 = trace "LDlc"     $ iLD l c
dispatch 0x6A = trace "LDld"     $ iLD l d
dispatch 0x6B = trace "LDle"     $ iLD l e
dispatch 0x6C = trace "LDlh"     $ iLD l h
dispatch 0x6D = trace "LDll"     $ iLD l l
dispatch 0x6E = trace "LDlHLm"   $ iLDHL l lHLm
dispatch 0x6F = trace "LDla"     $ iLD l a

dispatch 0x70 = trace "LDHLmb"    $ iLDHL lHLm b
dispatch 0x71 = trace "LDHLmc"    $ iLDHL lHLm c
dispatch 0x72 = trace "LDHLmd"    $ iLDHL lHLm d
dispatch 0x73 = trace "LDHLme"    $ iLDHL lHLm e
dispatch 0x74 = trace "LDHLmh"    $ iLDHL lHLm h
dispatch 0x75 = trace "LDHLml"    $ iLDHL lHLm l
dispatch 0x76 = trace "halt"      $ undefined
dispatch 0x77 = trace "LDHLmc"    $ iLDHL lHLm a

dispatch 0x78 = trace "LDlb"      $ iLD a b
dispatch 0x79 = trace "LDlc"      $ iLD a c
dispatch 0x7A = trace "LDld"      $ iLD a d
dispatch 0x7B = trace "LDle"      $ iLD a e
dispatch 0x7C = trace "LDlh"      $ iLD a h
dispatch 0x7D = trace "LDll"      $ iLD a l
dispatch 0x7E = trace "LDaHLm"    $ iLDHL a lHLm
dispatch 0x7F = trace "LDaa"      $ iLD a a

dispatch 0xA8 = trace "XORb"      $ iXOR b
dispatch 0xA9 = trace "XORc"      $ iXOR c
dispatch 0xAA = trace "XORd"      $ iXOR d
dispatch 0xAB = trace "XORe"      $ iXOR e
dispatch 0xAC = trace "XORh"      $ iXOR h
dispatch 0xAD = trace "XORl"      $ iXOR l
dispatch 0xAE = trace "XORHLm"    $ iXORHL lHLm
dispatch 0xAF = trace "XORa"      $ iXOR a

dispatch 0xB8 = trace "CPb"       $ iCPr_b >> mkClock 1 4

dispatch 0xC1 = trace "POPBC"     $ iPOP lBC
dispatch 0xC5 = trace "PUSHBC"    $ iPUSH lBC
dispatch 0xCB = trace "PrefCB"    $ iPrefCB

dispatch 0xD1 = trace "POPDE"     $ iPOP lDE
dispatch 0xD5 = trace "PUSHDE"    $ iPUSH lDE

dispatch 0xE1 = trace "POPHL"     $ iPOP lHL
dispatch 0xE5 = trace "PUSHHL"    $ iPUSH lHL

dispatch 0xF1 = trace "POPAF"     $ iPOP lAF
dispatch 0xF5 = trace "PUSHAF"    $ iPUSH lAF

dispatch op'   = error $ "Instruction not implemented: " ++ (printf "0x%02x" op')

-- | The instruction CB allow access to "bits instructions".
--   This function call the right bit instruction from the opcode.
dispatchCB :: Word8 -> Instruction

dispatchCB 0x40 = trace "BIT0b"   $ iBIT 0 b
dispatchCB 0x41 = trace "BIT0c"   $ iBIT 0 c
dispatchCB 0x42 = trace "BIT0d"   $ iBIT 0 d
dispatchCB 0x43 = trace "BIT0e"   $ iBIT 0 e
dispatchCB 0x44 = trace "BIT0h"   $ iBIT 0 h
dispatchCB 0x45 = trace "BIT0l"   $ iBIT 0 l
dispatchCB 0x46 = trace "BIT0HLm" $ iBITHL 0 lHLm
dispatchCB 0x47 = trace "BIT0a"   $ iBIT 0 a
dispatchCB 0x48 = trace "BIT0b"   $ iBIT 1 b
dispatchCB 0x49 = trace "BIT0c"   $ iBIT 1 c
dispatchCB 0x4A = trace "BIT0d"   $ iBIT 1 d
dispatchCB 0x4B = trace "BIT0e"   $ iBIT 1 e
dispatchCB 0x4C = trace "BIT0h"   $ iBIT 1 h
dispatchCB 0x4D = trace "BIT0l"   $ iBIT 1 l
dispatchCB 0x4E = trace "BIT0HLm" $ iBITHL 1 lHLm
dispatchCB 0x4F = trace "BIT0a"   $ iBIT 1 a

dispatchCB 0x50 = trace "BIT2b"   $ iBIT 2 b
dispatchCB 0x51 = trace "BIT2c"   $ iBIT 2 c
dispatchCB 0x52 = trace "BIT2d"   $ iBIT 2 d
dispatchCB 0x53 = trace "BIT2e"   $ iBIT 2 e
dispatchCB 0x54 = trace "BIT2h"   $ iBIT 2 h
dispatchCB 0x55 = trace "BIT2l"   $ iBIT 2 l
dispatchCB 0x56 = trace "BIT2HLm" $ iBITHL 2 lHLm
dispatchCB 0x57 = trace "BIT2a"   $ iBIT 2 a
dispatchCB 0x58 = trace "BIT3b"   $ iBIT 3 b
dispatchCB 0x59 = trace "BIT3c"   $ iBIT 3 c
dispatchCB 0x5A = trace "BIT3d"   $ iBIT 3 d
dispatchCB 0x5B = trace "BIT3e"   $ iBIT 3 e
dispatchCB 0x5C = trace "BIT3h"   $ iBIT 3 h
dispatchCB 0x5D = trace "BIT3l"   $ iBIT 3 l
dispatchCB 0x5E = trace "BIT3HLm" $ iBITHL 3 lHLm
dispatchCB 0x5F = trace "BIT3a"   $ iBIT 3 a

dispatchCB 0x60 = trace "BIT4b"   $ iBIT 4 b
dispatchCB 0x61 = trace "BIT4c"   $ iBIT 4 c
dispatchCB 0x62 = trace "BIT4d"   $ iBIT 4 d
dispatchCB 0x63 = trace "BIT4e"   $ iBIT 4 e
dispatchCB 0x64 = trace "BIT4h"   $ iBIT 4 h
dispatchCB 0x65 = trace "BIT4l"   $ iBIT 4 l
dispatchCB 0x66 = trace "BIT4HLm" $ iBITHL 4 lHLm
dispatchCB 0x67 = trace "BIT4a"   $ iBIT 4 a
dispatchCB 0x68 = trace "BIT5b"   $ iBIT 5 b
dispatchCB 0x69 = trace "BIT5c"   $ iBIT 5 c
dispatchCB 0x6A = trace "BIT5d"   $ iBIT 5 d
dispatchCB 0x6B = trace "BIT5e"   $ iBIT 5 e
dispatchCB 0x6C = trace "BIT5h"   $ iBIT 5 h
dispatchCB 0x6D = trace "BIT5l"   $ iBIT 5 l
dispatchCB 0x6E = trace "BIT5HLm" $ iBITHL 5 lHLm
dispatchCB 0x6F = trace "BIT5a"   $ iBIT 5 a

dispatchCB 0x70 = trace "BIT6b"   $ iBIT 6 b
dispatchCB 0x71 = trace "BIT6c"   $ iBIT 6 c
dispatchCB 0x72 = trace "BIT6d"   $ iBIT 6 d
dispatchCB 0x73 = trace "BIT6e"   $ iBIT 6 e
dispatchCB 0x74 = trace "BIT6h"   $ iBIT 6 h
dispatchCB 0x75 = trace "BIT6l"   $ iBIT 6 l
dispatchCB 0x76 = trace "BIT6HLm" $ iBITHL 6 lHLm
dispatchCB 0x77 = trace "BIT6a"   $ iBIT 6 a
dispatchCB 0x78 = trace "BIT7b"   $ iBIT 7 b
dispatchCB 0x79 = trace "BIT7c"   $ iBIT 7 c
dispatchCB 0x7A = trace "BIT7d"   $ iBIT 7 d
dispatchCB 0x7B = trace "BIT7e"   $ iBIT 7 e
dispatchCB 0x7C = trace "BIT7h"   $ iBIT 7 h
dispatchCB 0x7D = trace "BIT7l"   $ iBIT 7 l
dispatchCB 0x7E = trace "BIT7HLm" $ iBITHL 7 lHLm
dispatchCB 0x7F = trace "BIT7a"   $ iBIT 7 a

dispatchCB op' = error $ "Pefix CB not implemented for: " ++ (printf "0x%02x" op')

-- | Reset the flags to 0
fReset :: VmS ()
fReset = f .= 0

-- | Create a Clock in the VmS monad
mkClock :: Word -> Word -> VmS Clock
mkClock mV tV = return $ Clock mV tV

-- | Add time to a clock and output in the VmS monad
addClock :: Word -> Word -> Clock -> Clock
addClock mV tV clk = (m +~ mV) $ (t +~ tV) $ clk

-- | No OPeration
iNOP :: VmS Clock
iNOP = mkClock 1 4

-- | LD instruction that can be used with any Lens' Vm b
--
--   Syntax : `LD Dst Src`
--
--   > LD Register <- Register
iLD :: ASetter' Registers b -> Getting b Registers b -> VmS Clock
iLD output input = registers . output <~ use (registers . input) >> mkClock 1 4


-- | Same as LD, but alow to use (HL) on the right / left
--
--   Syntax : `LD Dst Src`
--
--   > LD (Register | (HL) ) <- (Register | HL)
iLDHL :: ASetter' Vm b -> Getting b Vm b -> VmS Clock
iLDHL output input = output <~ use (input) >> mkClock 1 8

-- | Load a value from/to ((HL) | a) into ((HL) | a),
--   and then increment HL.
--
--   Syntax : `LDI Dst Src`
--
--   > LDI ((HL) | a) <- ((HL) | a)
iLDI :: ASetter' Vm b -> Getting b Vm b -> VmS Clock
iLDI = iLDmod 1

-- | Load a value from/to ((HL) | a) into ((HL) | a),
--   and then decrement HL.
--
--   Syntax : `LDI Dst Src`
--
--   > LDI ((HL) | a) <- ((HL) | a)
iLDD :: ASetter' Vm b -> Getting b Vm b -> VmS Clock
iLDD =iLDmod (-1)

-- | Implementation of LDD/LDI where mod should be (-1)/(+1)
iLDmod :: Word16 -> ASetter' Vm b -> Getting b Vm b -> VmS Clock
iLDmod mod' output input = do
  output <~ use (input)
  lRR h l += mod'
  mkClock 1 8

-- | LD (SP | HL | DE | BC) <- immediate Word16
iLDd16 :: ASetter' Vm Word16 -> VmS Clock
iLDd16 output = output <~ readProgramW >> (mkClock 3 12)

-- | LD Register <- immediate Word8
iLDd8 :: ASetter' Registers Word8 -> VmS Clock
iLDd8 output = registers . output <~ readProgramB >> (mkClock 2 8)

-- | LD Register <- immediate Word8
iLDHLd8 :: ASetter' Vm Word8 -> VmS Clock
iLDHLd8 output = output <~ readProgramB >> (mkClock 2 12)

-- | XOR the register 'a' with a register 'R' into 'a'
--   Syntax : `XOR Src`
iXOR :: Getting Word8 Registers Word8 -> VmS Clock
iXOR input = iXORHL (registers . input) >> mkClock 1 4

iPUSH :: Getting Word16 Registers Word16 -> VmS Clock
iPUSH input = do
  lSPm16 <~ use (registers . input)
  sp += 2
  mkClock 1 16

iPOP :: ASetter' Registers Word16 -> VmS Clock
iPOP output = do
  registers . output <~ use lSPm16
  sp -= 2
  mkClock 1 16


-- | XOR the register a with (register R | (HL)) into a
--  Syntax : `XOR Src`
iXORHL :: Getting Word8 Vm Word8 -> VmS Clock
iXORHL input = do
  fReset
  value <- use input
  a %= xor value
  mkClock 1 8

-- | CB Prefix. Load the next bit and dispatch it using dispatchCB.
iPrefCB :: VmS Clock
iPrefCB = do
  jmpAddr <- readProgramB
  addClock 1 4 <$> dispatchCB jmpAddr

-- | Read the byte pointed by CP and at it to SP
iJR :: VmS Clock
iJR = do
  byte <- readProgramB
  -- byte is a signed value
  case byte > 127 of
    False -> pc += (fromIntegral $ byte)
    True  -> pc -= (fromIntegral $ complement byte + 1)
  mkClock 2 12

iJRf :: Getting Bool Registers Bool -> VmS Clock
iJRf flag = do
  shouldJump <- use $ registers . flag
  case shouldJump of
    True  -> iJR
    False -> readProgramB >> mkClock 2 8

-- | The BIT instruction check if the nth bit is set, by setting/unsetting
--   the zero flag.
--
-- Syntax : BIT [0 .. 7] (Register | (HL))
--
-- Always set the N flag to 0 and the H lag to 1.
iBIT :: Int -> Getting Word8 Registers Word8 -> VmS Clock
iBIT n input = iBITHL n (registers . input) >> mkClock 2 8

iBITHL :: Int -> Getting Word8 Vm Word8 -> VmS Clock
iBITHL n input = do
  value <- use input
  lZf .= (0 == value .&. shiftL 1 n)
  lCf .= False >> lHf .= True
  mkClock 2 16

iLDCma :: VmS Clock
iLDCma = undefined

iLDaCm :: VmS Clock
iLDaCm = undefined

-- | Compare B to A and set the flags
iCPr_b :: VmS ()
iCPr_b = do
  a' <- use $ a
  b' <- use $ b
  let diff = a' - b'
  fReset
  lCf .= (a' < b')
  lZf .= (0 == diff)
  lHf .= True
