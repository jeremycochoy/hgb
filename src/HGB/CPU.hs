{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE Rank2Types      #-}
module HGB.CPU where

import           Data.Word (Word8, Word16, Word)
import           Control.Lens
import           Control.Monad.State
import           HGB.Types
import           HGB.MMU
import           HGB.GPU
import           HGB.Lens
import           Data.Bits hiding (bit)
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

-- | Execute until the clock increase by (at least) 70224.
execFrame :: VmS ()
execFrame = do
  time <- use t
  execFrame' (time + 70224)
  where
    execFrame' endTime = do
      exec
      currentTime <- use t
      if currentTime >= endTime
        then return ()
        else execFrame' endTime

-- | Execute exactly one instruction
exec :: VmS ()
exec = do
  interruptState <- use interrupt

  disableBios =<< use vm
  inst <- dispatch =<< readProgramB

  t += inst ^. t
  m += inst ^. m

  updateGPUmode (inst ^. t)

  switchInterrupt interruptState
  where
    disableBios vm'
      | (vm' ^. pc) >= 0x100 = biosEnabled .= False
      | otherwise = return ()
    switchInterrupt INextInstE = interrupt .= IEnabled
    switchInterrupt INextInstD = interrupt .= IDisabled
    switchInterrupt _ = return()

-- | Select the right instruction from the opcode.
dispatch :: Word8 -> Instruction
dispatch 0x00 = trace "NOP"      $ iNOP
dispatch 0x01 = trace "LDSPd16"  $ iLDd16 lBC
dispatch 0x02 = trace "LDBCma"   $ iLDHL lBCm a
dispatch 0x03 = trace "INCBC"    $ iINCr16 lBC
dispatch 0x04 = trace "INCb"     $ iINC b
dispatch 0x05 = trace "DECb"     $ iDEC b
dispatch 0x06 = trace "LDbd8"    $ iLDd8 b
dispatch 0x07 = trace "RLCa"     $ iRLC a
dispatch 0x0A = trace "LDaBCm"   $ iLDHL a lBCm
dispatch 0x0B = trace "DECBC"    $ iDECr16 lBC
dispatch 0x0C = trace "INCc"     $ iINC c
dispatch 0x0D = trace "DECc"     $ iDEC c
dispatch 0x0E = trace "LDcd8"    $ iLDd8 c
dispatch 0x0F = trace "RRCa"     $ iRRC a

dispatch 0x11 = trace "LDSPd16"  $ iLDd16 lDE
dispatch 0x12 = trace "LDDEma"   $ iLDHL lDEm a
dispatch 0x13 = trace "INCDE"    $ iINCr16 lDE
dispatch 0x14 = trace "INCd"     $ iINC d
dispatch 0x15 = trace "DECd"     $ iDEC d
dispatch 0x16 = trace "LDdd8"    $ iLDd8 d
dispatch 0x17 = trace "RLa"      $ iRL a
dispatch 0x18 = trace "JRr8"     $ iJR
dispatch 0x1A = trace "LDaDEm"   $ iLDHL a lDEm
dispatch 0x1B = trace "DECDE"    $ iDECr16 lDE
dispatch 0x1C = trace "INCe"     $ iINC e
dispatch 0x1D = trace "DECe"     $ iDEC e
dispatch 0x1E = trace "LDed8"    $ iLDd8 e
dispatch 0x1F = trace "RCa"      $ iRR a

dispatch 0x20 = trace "JRNZr8"   $ iJRf lNZf
dispatch 0x21 = trace "LDSPd16"  $ iLDd16 lHL
dispatch 0x22 = trace "LDIHLma"  $ iLDI lHLm a
dispatch 0x23 = trace "INCHL"    $ iINCr16 lHL
dispatch 0x24 = trace "INCh"     $ iINC h
dispatch 0x25 = trace "DECh"     $ iDEC h
dispatch 0x26 = trace "LDhd8"    $ iLDd8 h
dispatch 0x28 = trace "JRZr8"    $ iJRf lZf
dispatch 0x2A = trace "LDIaHLm"  $ iLDI a lHLm
dispatch 0x2B = trace "DECHL"    $ iDECr16 lHL
dispatch 0x2C = trace "INCl"     $ iINC l
dispatch 0x2D = trace "DECl"     $ iDEC l
dispatch 0x2E = trace "LDld8"    $ iLDd8 l

dispatch 0x30 = trace "JRNCr8"   $ iJRf lNCf
dispatch 0x31 = trace "LDSPd16"  $ iLDd16 sp
dispatch 0x32 = trace "LDDHLma"  $ iLDD lHLm a
dispatch 0x33 = trace "INCSP"    $ iINCr16 sp
dispatch 0x34 = trace "INHLm"    $ iINCHL lHLm
dispatch 0x35 = trace "DECHLm"   $ iDECHL lHLm
dispatch 0x36 = trace "LDHLmd8"  $ iLDHLd8 lHLm
dispatch 0x38 = trace "JRCr8"    $ iJRf lCf
dispatch 0x3A = trace "LDIaHLm"  $ iLDD a lHLm
dispatch 0x3B = trace "DECSP"    $ iDECr16 sp
dispatch 0x3C = trace "INCa"     $ iINC a
dispatch 0x3D = trace "DECa"     $ iDEC a
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

dispatch 0x80 = trace "ADDb"      $ iADD b
dispatch 0x81 = trace "ADDc"      $ iADD c
dispatch 0x82 = trace "ADDd"      $ iADD d
dispatch 0x83 = trace "ADDe"      $ iADD e
dispatch 0x84 = trace "ADDh"      $ iADD h
dispatch 0x85 = trace "ADDl"      $ iADD l
dispatch 0x86 = trace "ADDHLm"    $ iADDHL lHLm
dispatch 0x87 = trace "ADDa"      $ iADD a

dispatch 0x90 = trace "SUBb"      $ iSUB b
dispatch 0x91 = trace "SUBc"      $ iSUB c
dispatch 0x92 = trace "SUBd"      $ iSUB d
dispatch 0x93 = trace "SUBe"      $ iSUB e
dispatch 0x94 = trace "SUBh"      $ iSUB h
dispatch 0x95 = trace "SUBl"      $ iSUB l
dispatch 0x96 = trace "SUBHLm"    $ iSUBHL lHLm
dispatch 0x97 = trace "SUBa"      $ iSUB a

dispatch 0xA8 = trace "XORb"      $ iXOR b
dispatch 0xA9 = trace "XORc"      $ iXOR c
dispatch 0xAA = trace "XORd"      $ iXOR d
dispatch 0xAB = trace "XORe"      $ iXOR e
dispatch 0xAC = trace "XORh"      $ iXOR h
dispatch 0xAD = trace "XORl"      $ iXOR l
dispatch 0xAE = trace "XORHLm"    $ iXORHL lHLm
dispatch 0xAF = trace "XORa"      $ iXOR a

dispatch 0xB0 = trace "ORb"       $ iOR b
dispatch 0xB1 = trace "ORc"       $ iOR c
dispatch 0xB2 = trace "ORd"       $ iOR d
dispatch 0xB3 = trace "ORe"       $ iOR e
dispatch 0xB4 = trace "ORh"       $ iOR h
dispatch 0xB5 = trace "ORl"       $ iOR l
dispatch 0xB6 = trace "ORHLm"     $ iORHL lHLm
dispatch 0xB7 = trace "ORa"       $ iOR a
dispatch 0xB8 = trace "CPb"       $ iCP b
dispatch 0xB9 = trace "CPc"       $ iCP c
dispatch 0xBA = trace "CPd"       $ iCP d
dispatch 0xBB = trace "CPe"       $ iCP e
dispatch 0xBC = trace "CPh"       $ iCP h
dispatch 0xBD = trace "CPl"       $ iCP l
dispatch 0xBE = trace "CPHLm"     $ iCPHL lHLm
dispatch 0xBF = trace "CPa"       $ iCP a

dispatch 0xC0 = trace "RETNZ"     $ iRETf lNZf
dispatch 0xC1 = trace "POPBC"     $ iPOP lBC
dispatch 0xC2 = trace "JPNZ"      $ iJPf lNZf
dispatch 0xC3 = trace "JP"        $ iJP
dispatch 0xC4 = trace "CALLNZ"    $ iCALLf lNZf
dispatch 0xC5 = trace "PUSHBC"    $ iPUSH lBC
dispatch 0xC8 = trace "RETZ"      $ iRETf lZf
dispatch 0xC9 = trace "RET"       $ iRET
dispatch 0xCA = trace "JPZ"       $ iJPf lZf
dispatch 0xCB = trace "PrefCB"    $ iPrefCB
dispatch 0xCC = trace "CALLZ"     $ iCALLf lZf
dispatch 0xCD = trace "CALL"      $ iCALL

dispatch 0xD0 = trace "RETNC"     $ iRETf lNCf
dispatch 0xD1 = trace "POPDE"     $ iPOP lDE
dispatch 0xD2 = trace "JPNC"      $ iJPf lNCf
dispatch 0xD3 = trace "none"      $ iNone 0xD3
dispatch 0xD4 = trace "CALLNC"    $ iCALLf lNCf
dispatch 0xD5 = trace "PUSHDE"    $ iPUSH lDE
dispatch 0xD6 = trace "SUBd8"     $ iSUBd8
dispatch 0xD8 = trace "RETC"      $ iRETf lCf
dispatch 0xD9 = trace "RETI"      $ iRETI
dispatch 0xDA = trace "JPC"       $ iJPf lCf
dispatch 0xDB = trace "none"      $ iNone 0xDB
dispatch 0xDD = trace "none"      $ iNone 0xDD
dispatch 0xDC = trace "CALLC"     $ iCALLf lCf

dispatch 0xE0 = trace "LDa8a"     $ iLDa8a
dispatch 0xE1 = trace "POPHL"     $ iPOP lHL
dispatch 0xE2 = trace "LDCma"     $ iLDCma
dispatch 0xE3 = trace "none"      $ iNone 0xE3
dispatch 0xE5 = trace "PUSHHL"    $ iPUSH lHL
dispatch 0xE6 = trace "ADDd8"     $ iADDd8
dispatch 0xE9 = trace "JPHLm"     $ iJPHLm
dispatch 0xEE = trace "XORd8"     $ iXORd8
dispatch 0xEA = trace "LDa16a"    $ iLDa16a
dispatch 0xEB = trace "none"      $ iNone 0xEB
dispatch 0xEC = trace "none"      $ iNone 0xEC
dispatch 0xED = trace "none"      $ iNone 0xED

dispatch 0xF0 = trace "LDaa8"     $ iLDaa8
dispatch 0xF1 = trace "POPAF"     $ iPOP lAF
dispatch 0xF2 = trace "LDaCm"     $ iLDaCm
dispatch 0xF3 = trace "DI"        $ iDI
dispatch 0xF4 = trace "none"      $ iNone 0xF4
dispatch 0xF5 = trace "PUSHAF"    $ iPUSH lAF
dispatch 0xF6 = trace "ORd8"      $ iORd8
dispatch 0xFA = trace "LDaa16"    $ iLDaa16
dispatch 0xFB = trace "EI"        $ iEI
dispatch 0xFC = trace "none"      $ iNone 0xFC
dispatch 0xFD = trace "none"      $ iNone 0xFD
dispatch 0xFE = trace "CPd8"      $ iCPd8

dispatch op'   = error $ "Instruction not implemented: " ++ (printf "0x%02x" op')

-- | The instruction CB allow access to "bits instructions".
--   This function call the right bit instruction from the opcode.
dispatchCB :: Word8 -> Instruction

dispatchCB 0x00 = trace "RLCb"    $ iRLC b
dispatchCB 0x01 = trace "RLCc"    $ iRLC c
dispatchCB 0x02 = trace "RLCd"    $ iRLC d
dispatchCB 0x03 = trace "RLCe"    $ iRLC e
dispatchCB 0x04 = trace "RLCh"    $ iRLC h
dispatchCB 0x05 = trace "RLCl"    $ iRLC l
dispatchCB 0x06 = trace "RLCHLm"  $ iRLCHL lHLm
dispatchCB 0x07 = trace "RLCa"    $ iRLC a
dispatchCB 0x08 = trace "RRCb"    $ iRRC b
dispatchCB 0x09 = trace "RRCc"    $ iRRC c
dispatchCB 0x0A = trace "RRCd"    $ iRRC d
dispatchCB 0x0B = trace "RRCe"    $ iRRC e
dispatchCB 0x0C = trace "RRCh"    $ iRRC h
dispatchCB 0x0D = trace "RRCl"    $ iRRC l
dispatchCB 0x0E = trace "RRCHLm"  $ iRRCHL lHLm
dispatchCB 0x0F = trace "RRCa"    $ iRRC a

dispatchCB 0x10 = trace "RLb"     $ iRL b
dispatchCB 0x11 = trace "RLc"     $ iRL c
dispatchCB 0x12 = trace "RLd"     $ iRL d
dispatchCB 0x13 = trace "RLe"     $ iRL e
dispatchCB 0x14 = trace "RLh"     $ iRL h
dispatchCB 0x15 = trace "RLl"     $ iRL l
dispatchCB 0x16 = trace "RLHLm"   $ iRLHL lHLm
dispatchCB 0x17 = trace "RLa"     $ iRL a
dispatchCB 0x18 = trace "RRb"     $ iRR b
dispatchCB 0x19 = trace "RRc"     $ iRR c
dispatchCB 0x1A = trace "RRd"     $ iRR d
dispatchCB 0x1B = trace "RRe"     $ iRR e
dispatchCB 0x1C = trace "RRh"     $ iRR h
dispatchCB 0x1D = trace "RRl"     $ iRR l
dispatchCB 0x1E = trace "RRHLm"   $ iRRHL lHLm
dispatchCB 0x1F = trace "RRa"     $ iRR a

dispatchCB 0x20 = trace "SLAb"    $ iSLA b
dispatchCB 0x21 = trace "SLAc"    $ iSLA c
dispatchCB 0x22 = trace "SLAd"    $ iSLA d
dispatchCB 0x23 = trace "SLAe"    $ iSLA e
dispatchCB 0x24 = trace "SLAh"    $ iSLA h
dispatchCB 0x25 = trace "SLAl"    $ iSLA l
dispatchCB 0x26 = trace "SLAHLm"  $ iSLAHL lHLm
dispatchCB 0x27 = trace "SLAa"    $ iSLA a
dispatchCB 0x28 = trace "SRAb"    $ iSRA b
dispatchCB 0x29 = trace "SRAc"    $ iSRA c
dispatchCB 0x2A = trace "SRAd"    $ iSRA d
dispatchCB 0x2B = trace "SRAe"    $ iSRA e
dispatchCB 0x2C = trace "SRAh"    $ iSRA h
dispatchCB 0x2D = trace "SRAl"    $ iSRA l
dispatchCB 0x2E = trace "SRAHLm"  $ iSRAHL lHLm
dispatchCB 0x2F = trace "SRAa"    $ iSRA a

dispatchCB 0x30 = trace "SWAPb"   $ iSWAP b
dispatchCB 0x31 = trace "SWAPc"   $ iSWAP c
dispatchCB 0x32 = trace "SWAPd"   $ iSWAP d
dispatchCB 0x33 = trace "SWAPe"   $ iSWAP e
dispatchCB 0x34 = trace "SWAPh"   $ iSWAP h
dispatchCB 0x35 = trace "SWAPl"   $ iSWAP l
dispatchCB 0x36 = trace "SWAPHLm" $ iSWAPHL lHLm
dispatchCB 0x37 = trace "SWAPa"   $ iSWAP a
dispatchCB 0x38 = trace "SRAb"    $ iSRL b
dispatchCB 0x39 = trace "SRAc"    $ iSRL c
dispatchCB 0x3A = trace "SRAd"    $ iSRL d
dispatchCB 0x3B = trace "SRAe"    $ iSRL e
dispatchCB 0x3C = trace "SRAh"    $ iSRL h
dispatchCB 0x3D = trace "SRAl"    $ iSRL l
dispatchCB 0x3E = trace "SRAHLm"  $ iSRLHL lHLm
dispatchCB 0x3F = trace "SRAa"    $ iSRL a

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

dispatchCB 0x80 = trace "RES0b"   $ iRES 0 b
dispatchCB 0x81 = trace "RES0c"   $ iRES 0 c
dispatchCB 0x82 = trace "RES0d"   $ iRES 0 d
dispatchCB 0x83 = trace "RES0e"   $ iRES 0 e
dispatchCB 0x84 = trace "RES0h"   $ iRES 0 h
dispatchCB 0x85 = trace "RES0l"   $ iRES 0 l
dispatchCB 0x86 = trace "RES0HLm" $ iRESHL 0 lHLm
dispatchCB 0x87 = trace "RES0a"   $ iRES 0 a
dispatchCB 0x88 = trace "RES1b"   $ iRES 1 b
dispatchCB 0x89 = trace "RES1c"   $ iRES 1 c
dispatchCB 0x8A = trace "RES1d"   $ iRES 1 d
dispatchCB 0x8B = trace "RES1e"   $ iRES 1 e
dispatchCB 0x8C = trace "RES1h"   $ iRES 1 h
dispatchCB 0x8D = trace "RES1l"   $ iRES 1 l
dispatchCB 0x8E = trace "RES1HLm" $ iRESHL 1 lHLm
dispatchCB 0x8F = trace "RES1a"   $ iRES 1 a

dispatchCB 0x90 = trace "RES2b"   $ iRES 2 b
dispatchCB 0x91 = trace "RES2c"   $ iRES 2 c
dispatchCB 0x92 = trace "RES2d"   $ iRES 2 d
dispatchCB 0x93 = trace "RES2e"   $ iRES 2 e
dispatchCB 0x94 = trace "RES2h"   $ iRES 2 h
dispatchCB 0x95 = trace "RES2l"   $ iRES 2 l
dispatchCB 0x96 = trace "RES2HLm" $ iRESHL 2 lHLm
dispatchCB 0x97 = trace "RES2a"   $ iRES 2 a
dispatchCB 0x98 = trace "RES3b"   $ iRES 3 b
dispatchCB 0x99 = trace "RES3c"   $ iRES 3 c
dispatchCB 0x9A = trace "RES3d"   $ iRES 3 d
dispatchCB 0x9B = trace "RES3e"   $ iRES 3 e
dispatchCB 0x9C = trace "RES3h"   $ iRES 3 h
dispatchCB 0x9D = trace "RES3l"   $ iRES 3 l
dispatchCB 0x9E = trace "RES3HLm" $ iRESHL 3 lHLm
dispatchCB 0x9F = trace "RES3a"   $ iRES 3 a

dispatchCB 0xA0 = trace "RES4b"   $ iRES 4 b
dispatchCB 0xA1 = trace "RES4c"   $ iRES 4 c
dispatchCB 0xA2 = trace "RES4d"   $ iRES 4 d
dispatchCB 0xA3 = trace "RES4e"   $ iRES 4 e
dispatchCB 0xA4 = trace "RES4h"   $ iRES 4 h
dispatchCB 0xA5 = trace "RES4l"   $ iRES 4 l
dispatchCB 0xA6 = trace "RES4HLm" $ iRESHL 4 lHLm
dispatchCB 0xA7 = trace "RES4a"   $ iRES 4 a
dispatchCB 0xA8 = trace "RES5b"   $ iRES 5 b
dispatchCB 0xA9 = trace "RES5c"   $ iRES 5 c
dispatchCB 0xAA = trace "RES5d"   $ iRES 5 d
dispatchCB 0xAB = trace "RES5e"   $ iRES 5 e
dispatchCB 0xAC = trace "RES5h"   $ iRES 5 h
dispatchCB 0xAD = trace "RES5l"   $ iRES 5 l
dispatchCB 0xAE = trace "RES5HLm" $ iRESHL 5 lHLm
dispatchCB 0xAF = trace "RES5a"   $ iRES 5 a

dispatchCB 0xB0 = trace "RES6b"   $ iRES 6 b
dispatchCB 0xB1 = trace "RES6c"   $ iRES 6 c
dispatchCB 0xB2 = trace "RES6d"   $ iRES 6 d
dispatchCB 0xB3 = trace "RES6e"   $ iRES 6 e
dispatchCB 0xB4 = trace "RES6h"   $ iRES 6 h
dispatchCB 0xB5 = trace "RES6l"   $ iRES 6 l
dispatchCB 0xB6 = trace "RES6HLm" $ iRESHL 6 lHLm
dispatchCB 0xB7 = trace "RES6a"   $ iRES 6 a
dispatchCB 0xB8 = trace "RES7b"   $ iRES 7 b
dispatchCB 0xB9 = trace "RES7c"   $ iRES 7 c
dispatchCB 0xBA = trace "RES7d"   $ iRES 7 d
dispatchCB 0xBB = trace "RES7e"   $ iRES 7 e
dispatchCB 0xBC = trace "RES7h"   $ iRES 7 h
dispatchCB 0xBD = trace "RES7l"   $ iRES 7 l
dispatchCB 0xBE = trace "RES7HLm" $ iRESHL 7 lHLm
dispatchCB 0xBF = trace "RES7a"   $ iRES 7 a

dispatchCB 0xC0 = trace "SET0b"   $ iSET 0 b
dispatchCB 0xC1 = trace "SET0c"   $ iSET 0 c
dispatchCB 0xC2 = trace "SET0d"   $ iSET 0 d
dispatchCB 0xC3 = trace "SET0e"   $ iSET 0 e
dispatchCB 0xC4 = trace "SET0h"   $ iSET 0 h
dispatchCB 0xC5 = trace "SET0l"   $ iSET 0 l
dispatchCB 0xC6 = trace "SET0HLm" $ iSETHL 0 lHLm
dispatchCB 0xC7 = trace "SET0a"   $ iSET 0 a
dispatchCB 0xC8 = trace "SET1b"   $ iSET 1 b
dispatchCB 0xC9 = trace "SET1c"   $ iSET 1 c
dispatchCB 0xCA = trace "SET1d"   $ iSET 1 d
dispatchCB 0xCB = trace "SET1e"   $ iSET 1 e
dispatchCB 0xCC = trace "SET1h"   $ iSET 1 h
dispatchCB 0xCD = trace "SET1l"   $ iSET 1 l
dispatchCB 0xCE = trace "SET1HLm" $ iSETHL 1 lHLm
dispatchCB 0xCF = trace "SET1a"   $ iSET 1 a

dispatchCB 0xD0 = trace "SET2b"   $ iSET 2 b
dispatchCB 0xD1 = trace "SET2c"   $ iSET 2 c
dispatchCB 0xD2 = trace "SET2d"   $ iSET 2 d
dispatchCB 0xD3 = trace "SET2e"   $ iSET 2 e
dispatchCB 0xD4 = trace "SET2h"   $ iSET 2 h
dispatchCB 0xD5 = trace "SET2l"   $ iSET 2 l
dispatchCB 0xD6 = trace "SET2HLm" $ iSETHL 2 lHLm
dispatchCB 0xD7 = trace "SET2a"   $ iSET 2 a
dispatchCB 0xD8 = trace "SET3b"   $ iSET 3 b
dispatchCB 0xD9 = trace "SET3c"   $ iSET 3 c
dispatchCB 0xDA = trace "SET3d"   $ iSET 3 d
dispatchCB 0xDB = trace "SET3e"   $ iSET 3 e
dispatchCB 0xDC = trace "SET3h"   $ iSET 3 h
dispatchCB 0xDD = trace "SET3l"   $ iSET 3 l
dispatchCB 0xDE = trace "SET3HLm" $ iSETHL 3 lHLm
dispatchCB 0xDF = trace "SET3a"   $ iSET 3 a

dispatchCB 0xE0 = trace "SET4b"   $ iSET 4 b
dispatchCB 0xE1 = trace "SET4c"   $ iSET 4 c
dispatchCB 0xE2 = trace "SET4d"   $ iSET 4 d
dispatchCB 0xE3 = trace "SET4e"   $ iSET 4 e
dispatchCB 0xE4 = trace "SET4h"   $ iSET 4 h
dispatchCB 0xE5 = trace "SET4l"   $ iSET 4 l
dispatchCB 0xE6 = trace "SET4HLm" $ iSETHL 4 lHLm
dispatchCB 0xE7 = trace "SET4a"   $ iSET 4 a
dispatchCB 0xE8 = trace "SET5b"   $ iSET 5 b
dispatchCB 0xE9 = trace "SET5c"   $ iSET 5 c
dispatchCB 0xEA = trace "SET5d"   $ iSET 5 d
dispatchCB 0xEB = trace "SET5e"   $ iSET 5 e
dispatchCB 0xEC = trace "SET5h"   $ iSET 5 h
dispatchCB 0xED = trace "SET5l"   $ iSET 5 l
dispatchCB 0xEE = trace "SET5HLm" $ iSETHL 5 lHLm
dispatchCB 0xEF = trace "SET5a"   $ iSET 5 a

dispatchCB 0xF0 = trace "SET6b"   $ iSET 6 b
dispatchCB 0xF1 = trace "SET6c"   $ iSET 6 c
dispatchCB 0xF2 = trace "SET6d"   $ iSET 6 d
dispatchCB 0xF3 = trace "SET6e"   $ iSET 6 e
dispatchCB 0xF4 = trace "SET6h"   $ iSET 6 h
dispatchCB 0xF5 = trace "SET6l"   $ iSET 6 l
dispatchCB 0xF6 = trace "SET6HLm" $ iSETHL 6 lHLm
dispatchCB 0xF7 = trace "SET6a"   $ iSET 6 a
dispatchCB 0xF8 = trace "SET7b"   $ iSET 7 b
dispatchCB 0xF9 = trace "SET7c"   $ iSET 7 c
dispatchCB 0xFA = trace "SET7d"   $ iSET 7 d
dispatchCB 0xFB = trace "SET7e"   $ iSET 7 e
dispatchCB 0xFC = trace "SET7h"   $ iSET 7 h
dispatchCB 0xFD = trace "SET7l"   $ iSET 7 l
dispatchCB 0xFE = trace "SET7HLm" $ iSETHL 7 lHLm
dispatchCB 0xFF = trace "SET7a"   $ iSET 7 a

dispatchCB op' = error $ "Invalid Word8 in call to dispatchCB: " ++ (printf "0x%02x" op')

-- | Reset the flags to 0
fReset :: VmS ()
fReset = f .= 0

-- | Create a Clock in the VmS monad
mkClock :: Word -> Word -> VmS Clock
mkClock mV tV = return $ Clock mV tV

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

-- | LD (a16) <- a where a16 means the next Word16 as an address.
iLDa16a :: VmS Clock
iLDa16a = do
  addr <- readProgramW
  la16 addr <~ use a
  mkClock 3 12

-- | LD a <- (a16) where a16 means the next Word16 as an address.
iLDaa16 :: VmS Clock
iLDaa16 = do
  addr <- readProgramW
  a <~ use (la16 addr)
  mkClock 3 12

-- | LD (a8) <- a where a8 means the next Word8 + 0xFF00 as an address.
iLDa8a :: VmS Clock
iLDa8a = do
  addr <- readProgramB
  la8 addr <~ use a
  mkClock 3 12

-- | LD a <- (a8) where a8 means the next Word8 + 0xFF00 as an address.
iLDaa8 :: VmS Clock
iLDaa8 = do
  addr <- readProgramB
  a <~ use (la8 addr)
  mkClock 3 12

-- | XOR the register 'a' with a register 'R' into 'a'
--   Syntax : `XOR Src`
iXOR :: Getting Word8 Registers Word8 -> VmS Clock
iXOR input = (use $ registers . input) >>= iXORimp >> mkClock 1 4

-- | XOR the register a with (register R | (HL)) into a
--  Syntax : `XOR Src`
iXORHL :: Getting Word8 Vm Word8 -> VmS Clock
iXORHL input = (use input) >>= iXORimp >> mkClock 1 8

-- | XOR the register 'a' with the immediate Word8
--   Syntax : `XOR Src`
iXORd8 :: VmS Clock
iXORd8 = readProgramB >>= iXORimp >> mkClock 2 8

iXORimp :: Word8 -> VmS ()
iXORimp value = do
  fReset
  res <- a <%= xor value
  lZf .= (res == 0)

-- | Bitwise OR the register 'a' with a register 'R' into 'a'
--   Syntax : `OR Src`
iOR :: Getting Word8 Registers Word8 -> VmS Clock
iOR input = (use $ registers . input) >>= iORimp >> mkClock 1 4

-- | Bitwise OR the register a with (register R | (HL)) into a
--  Syntax : `OR Src`
iORHL :: Getting Word8 Vm Word8 -> VmS Clock
iORHL input = (use input) >>= iORimp >> mkClock 1 8

-- | Bitwise OR the register 'a' with the immediate Word8
--   Syntax : `OR Src`
iORd8 :: VmS Clock
iORd8 = readProgramB >>= iORimp >> mkClock 2 8

iORimp :: Word8 -> VmS ()
iORimp value = do
  fReset
  res <- a <%= (.|. value)
  lZf .= (res == 0)

-- | Increment the register given, and set Z, H as expected.
--   Always set N to 0.
--
-- Syntax : `INC reg`
iINC :: Lens' Registers Word8 -> VmS Clock
iINC field = iINCHL (registers . field) >> mkClock 1 4

-- | Increment the 16 bits registers given. Do not change any flag.
iINCr16 :: Lens' Registers Word16 -> VmS Clock
iINCr16 field = (registers . field) <+= 1 >> mkClock 1 8

-- | Increment (HL). Flag behave as INC does.
iINCHL :: Lens' Vm Word8 -> VmS Clock
iINCHL field = do
  initial <- use field
  res     <- field <+= 1
  lZf .= (res == 0)
  lHf .= (initial .&. 0xF == 0xF)
  lNf .= False
  mkClock 1 12

-- | Decrement the register given, and set Z, H as expected.
--   Always set N to 0.
--
-- Syntax : `DEC reg`
iDEC :: Lens' Registers Word8 -> VmS Clock
iDEC field = iDECHL (registers . field) >> mkClock 1 4

-- | Decrement the 16 bits registers given. Do not change any flag.
iDECr16 :: Lens' Registers Word16 -> VmS Clock
iDECr16 field = (registers . field) <-= 1 >> mkClock 1 8

-- | Decrement (HL). Flag behave as INC does.
iDECHL :: Lens' Vm Word8 -> VmS Clock
iDECHL field = do
  initial <- use field
  res     <- field <-= 1
  lZf .= (res == 0)
  lHf .= (initial .&. 0xF == 0)
  lNf .= True
  mkClock 1 12

-- | Push the value on the stack
iPUSH :: Getting Word16 Registers Word16 -> VmS Clock
iPUSH input = do
  sp -= 2
  lSPm16 <~ use (registers . input)
  mkClock 1 16

-- | Pop the value from the stack
iPOP :: ASetter' Registers Word16 -> VmS Clock
iPOP output = do
  registers . output <~ use lSPm16
  sp += 2
  mkClock 1 16

-- | CB Prefix. Load the next bit and dispatch it using dispatchCB.
iPrefCB :: VmS Clock
iPrefCB = do
  jmpAddr <- readProgramB
  -- Prefix 0xCB byte and cycles are already counted in dispatchCB's return.
  dispatchCB jmpAddr

-- | Read the byte pointed by CP and add it to CP
iJR :: VmS Clock
iJR = do
  byte <- readProgramB
  -- byte is a signed value
  case byte > 127 of
    False -> pc += (fromIntegral $ byte)
    True  -> pc -= (fromIntegral $ complement byte + 1)
  mkClock 2 12

-- | Conditional JR
iJRf :: Getting Bool Registers Bool -> VmS Clock
iJRf flag = do
  shouldJump <- use $ registers . flag
  case shouldJump of
    True  -> iJR
    False -> readProgramB >> mkClock 2 8

-- | Read the two bytes pointed by CP and place them into CP
iJP :: VmS Clock
iJP = do
  addr <- readProgramW
  pc .= addr
  mkClock 3 16

-- | JP (HL)
iJPHLm :: VmS Clock
iJPHLm = do
  addr <- use lHL
  pc .= addr
  mkClock 1 4

-- | Conditional JP
iJPf :: Getting Bool Registers Bool -> VmS Clock
iJPf flag = do
  shouldJump <- use $ registers . flag
  case shouldJump of
    True  -> iJP
    False -> readProgramW >> mkClock 3 12

-- | Push CP and load the immediate Word16 address into CP
iCALL :: VmS Clock
iCALL = do
  addr <- readProgramW
  _ <- iPUSH pc
  pc .= addr
  mkClock 3 16

-- | Conditional JP
iCALLf :: Getting Bool Registers Bool -> VmS Clock
iCALLf flag = do
  shouldJump <- use $ registers . flag
  case shouldJump of
    True  -> iCALL
    False -> readProgramW >> mkClock 3 12

-- | Push CP and load the immediate Word16 address into CP
iRET :: VmS Clock
iRET = iPOP pc >> mkClock 1 16

-- | Conditional JP
iRETf :: Getting Bool Registers Bool -> VmS Clock
iRETf flag = do
  shouldJump <- use $ registers . flag
  case shouldJump of
    True  -> iPOP pc >> mkClock 1 20
    False -> mkClock 1 8

-- | Same as ret, but enable interrupt after returning
iRETI :: VmS Clock
iRETI = iRET >> interrupt .= IEnabled >> mkClock 1 16

-- | Enable Interrupts
--
--   Interuptions will be enabled after the following instruction
--   was executed.
iEI :: VmS Clock
iEI = interrupt .= INextInstE >> mkClock 1 4

iDI :: VmS Clock
iDI = interrupt .= INextInstD >> mkClock 1 4

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
  lZf .= testBit value n
  lCf .= False >> lHf .= True
  mkClock 2 16

iLDCma :: VmS Clock
iLDCma = do
  addr <- ((+ 0xFF00) . fromIntegral) `liftM` use c
  value <- use a
  mmu %= wb addr value
  mkClock 1 8

iLDaCm :: VmS Clock
iLDaCm = do
  addr <- ((+ 0xFF00) . fromIntegral) `liftM` use c
  a <~ rb addr `liftM` use mmu
  mkClock 1 8

iSWAP :: Lens' Registers Word8 -> VmS Clock
iSWAP io = iSWAPHL (registers . io) >> mkClock 2 8

iSWAPHL :: Lens' Vm Word8 -> VmS Clock
iSWAPHL io = do
  value <- io <%= swap
  fReset
  lZf .= (0 == value)
  mkClock 2 16

iSET :: Int -> ASetter' Registers Word8 -> VmS Clock
iSET bit output = (registers . output %= (flip setBit bit)) >> mkClock 2 8

iSETHL :: Int -> ASetter' Vm Word8 -> VmS Clock
iSETHL bit output = (output %= (flip setBit bit)) >> mkClock 2 16

iRES :: Int -> ASetter' Registers Word8 -> VmS Clock
iRES bit output = (registers . output) %= (flip clearBit bit) >> mkClock 2 8

iRESHL :: Int -> ASetter' Vm Word8 -> VmS Clock
iRESHL bit output = output %= (flip clearBit bit) >> mkClock 2 16

-- | Rotate left and place 7th bit into carry.
iRLC :: Lens' Registers Word8 -> VmS Clock
iRLC io = iRLCHL (registers . io) >> mkClock 2 8

iRLCHL :: Lens' Vm Word8 -> VmS Clock
iRLCHL io = do
  value <- use io
  res <- io <.= rotate value 1 -- Rotate left
  fReset
  lCf .= (value .&. 0x80 /= 0) -- Take old bit 7
  lZf .= (res == 0)
  mkClock 2 16

-- | Rotate left and place 7th bit into carry.
iRRC :: Lens' Registers Word8 -> VmS Clock
iRRC io = iRRCHL (registers . io) >> mkClock 2 8

iRRCHL :: Lens' Vm Word8 -> VmS Clock
iRRCHL io = do
  value <- use io
  res <- io <.= rotate value (-1) -- Rotate right
  fReset
  lCf .= (value .&. 0x01 /= 0) -- Take old bit 0
  lZf .= (res == 0)
  mkClock 2 16

-- | Rotate left throught carry
--
--  7th bit go into carry, carry bit is now the lowest byte.
iRL :: Lens' Registers Word8 -> VmS Clock
iRL io = iRLHL (registers . io) >> mkClock 2 8

iRLHL :: Lens' Vm Word8 -> VmS Clock
iRLHL io = do
  value <- use io
  bit <- (\case False -> 0 ; True -> 1) `liftM` use lCf
  res <- io <.= (shiftL value 1) .|. bit
  fReset
  lCf .= (value .&. 0x80 /= 0) -- Take old bit 7
  lZf .= (res == 0)
  mkClock 2 16

-- | Rotate right throught carry
--
--  0th bit go into carry, carry bit is now the highest byte.
iRR :: Lens' Registers Word8 -> VmS Clock
iRR io = iRRHL (registers . io) >> mkClock 2 8

iRRHL :: Lens' Vm Word8 -> VmS Clock
iRRHL io = do
  value <- use io
  bit <- (\case False -> 0 ; True -> 1) `liftM` use lCf
  res <- io <.= (shiftR value 1) .|. (shiftL bit 7)
  fReset
  lCf .= (value .&. 0x01 /= 0) -- Take old bit 0
  lZf .= (res == 0)
  mkClock 2 16

-- | Shift Left to carry
--
--  Shift left. The 7th bit is placed into carry, the 0th is now 0.
iSLA :: Lens' Registers Word8 -> VmS Clock
iSLA io = iSLAHL (registers . io) >> mkClock 2 8

iSLAHL :: Lens' Vm Word8 -> VmS Clock
iSLAHL io = do
  value <- use io
  res <- io <.= (shiftL value 1)
  fReset
  lCf .= (value .&. 0x80 /= 0) -- Take old bit 7
  lZf .= (res == 0)
  mkClock 2 16

-- | Shift Right to carry
--
--  Shift Right. The 7th bit is unchanged, the 0th is placed into carry.
iSRA :: Lens' Registers Word8 -> VmS Clock
iSRA io = iSRAHL (registers . io) >> mkClock 2 8

iSRAHL :: Lens' Vm Word8 -> VmS Clock
iSRAHL io = do
  value <- use io
  res <- io <.= (shiftR value 1) .|. (value .&. 0x80)
  fReset
  lCf .= (value .&. 0x01 /= 0) -- Take old bit 0
  lZf .= (res == 0)
  mkClock 2 16

-- | Shift right to carry
--
--  Shift Right. The 7th bit is 0, the 0th is placed into carry.
iSRL :: Lens' Registers Word8 -> VmS Clock
iSRL io = iSRLHL (registers . io) >> mkClock 2 8

iSRLHL :: Lens' Vm Word8 -> VmS Clock
iSRLHL io = do
  value <- use io
  res <- io <.= (shiftR value 1)
  fReset
  lCf .= (value .&. 0x01 /= 0) -- Take old bit 0
  lZf .= (res == 0)
  mkClock 2 16

-- | Compare input to A and set the flags Z/H/C, reset N.
iCP :: Getting Word8 Registers Word8 -> VmS Clock
iCP input = use (registers . input) >>= iSUBimp >> mkClock 1 4

-- | Compare (HL) to A and set the flags Z/H/C, reset N.
iCPHL :: Getting Word8 Vm Word8 -> VmS Clock
iCPHL input = use (input) >>= iSUBimp >> mkClock 1 8

-- | Compare (HL) to A and set the flags Z/H/C, reset N.
iCPd8 :: VmS Clock
iCPd8 = readProgramB >>= iSUBimp >> mkClock 2 8

-- | Substract the input value to A, and place the result in A.
--
--   Set the right flags, reset N.
--
--   Syntax : A <- A - input
iSUB :: Getting Word8 Registers Word8 -> VmS Clock
iSUB input = a <~ (iSUBimp =<< use (registers . input)) >> mkClock 1 4

iSUBHL :: Getting Word8 Vm Word8 -> VmS Clock
iSUBHL input = a <~ (iSUBimp =<< use input) >> mkClock 1 8

iSUBd8 :: VmS Clock
iSUBd8 = a <~ (iSUBimp =<< readProgramB) >> mkClock 2 8

iSUBimp :: Word8 -> VmS Word8
iSUBimp value = do
  a' <- use $ a
  let b' = value
  let diff = a' - b'
  fReset
  lZf .= (0 == diff)
  lNf .= True
  lHf .= ((0x0F.&.a' - 0x0F.&.b') .&. 0x10 /= 0)
  lCf .= (a' < b')
  return diff

-- | Add the input value to A, and place the result in A.
--
--   Set the right flags, reset N.
--
--   Syntax : A <- A - input
iADD :: Getting Word8 Registers Word8 -> VmS Clock
iADD input = a <~ (iADDimp =<< use (registers . input)) >> mkClock 1 4

iADDHL :: Getting Word8 Vm Word8 -> VmS Clock
iADDHL input = a <~ (iADDimp =<< use input) >> mkClock 1 8

iADDd8 :: VmS Clock
iADDd8 = a <~ (iADDimp =<< readProgramB) >> mkClock 2 8

iADDimp :: Word8 -> VmS Word8
iADDimp value = do
  a' <- use $ a
  let b' = value
  let sum = a' + b'
  fReset
  lZf .= (0 == sum)
  lNf .= False
  lHf .= ((0x0F.&.a' + 0x0F.&.b') .&. 0x10 /= 0)
  lCf .= (<) (0xFF :: Int) (fromIntegral a' + fromIntegral b')
  return sum

iNone :: Word8 -> VmS Clock
iNone op' = error $ "This instruction desn't exists: " ++ (printf "0x%02x" op')
