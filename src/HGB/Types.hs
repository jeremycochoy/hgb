{-# LANGUAGE TemplateHaskell #-}

module HGB.Types where

import           Data.Word (Word8(..), Word16(..), Word(..))
import           Control.Lens
import           Data.Default
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed ((!))
import           Control.Monad.State
import           Data.Bits

-- | Set of register
data Registers = Registers
  { _a  :: !Word8
  , _b  :: !Word8
  , _c  :: !Word8
  , _d  :: !Word8
  , _e  :: !Word8
  , _h  :: !Word8
  , _l  :: !Word8
  , _f  :: !Word8
    -- ^ Flags
  , _pc :: !Word16
    -- ^ Program Counter
  , _sp :: !Word16
    -- ^ Stack Pointer
  } deriving (Show, Eq)


instance Default Registers where
  def = Registers
    { _a = 0
    , _b = 0
    , _c = 0
    , _d = 0
    , _e = 0
    , _h = 0
    , _l = 0
    , _f = 0
    , _pc = 0
    , _sp = 0
    }

-- | The internal clock
data Clock = Clock
  { _m :: !Word
  , _t :: !Word
  } deriving (Show, Eq)


instance Default Clock where
  def = Clock
    { _m = 0
    , _t = 0
    }

-- | The Z80 CPU
data Cpu = Cpu { _cpuRegisters :: !Registers, _cpuClock :: !Clock }
           deriving (Show, Eq)


instance Default Cpu where
  def = Cpu
    { _cpuRegisters = def
    , _cpuClock = def
    }

{-
From: Pan Docs - nocash / kOOPa

General Memory Map

  0000-3FFF   16KB ROM Bank 00     (in cartridge, fixed at bank 00)
  4000-7FFF   16KB ROM Bank 01..NN (in cartridge, switchable bank number)
  8000-9FFF   8KB Video RAM (VRAM) (switchable bank 0-1 in CGB Mode)
  A000-BFFF   8KB External RAM     (in cartridge, switchable bank, if any)
  C000-CFFF   4KB Work RAM Bank 0 (WRAM)
  D000-DFFF   4KB Work RAM Bank 1 (WRAM)  (switchable bank 1-7 in CGB Mode)
  E000-FDFF   Same as C000-DDFF (ECHO)    (typically not used)
  FE00-FE9F   Sprite Attribute Table (OAM)
  FEA0-FEFF   Not Usable
  FF00-FF7F   I/O Ports
  FF80-FFFE   High RAM (HRAM)
  FFFF        Interrupt Enable Register
-}

-- | The MMU (memory)
data Mmu = Mmu
  { _bios  :: !(V.Vector Word8)
  -- GB Bios
  , _rom   :: !(V.Vector Word8)
  -- ^ 0000-3FFF   16KB ROM Bank 00
  , _srom  :: !(V.Vector Word8)
  -- ^ 4000-7FFF   16KB ROM Bank 01
  , _eram  :: !(V.Vector Word8)
  -- ^ A000-BFFF   8KB External RAM
  , _wram  :: !(V.Vector Word8)
  -- ^ C000-CFFF   4KB Work RAM Bank 0 (WRAM)
  , _swram :: !(V.Vector Word8)
  -- ^ D000-DFFF   4KB Work RAM Bank 1 (WRAM)

  } deriving (Show, Eq)

instance Default Mmu where
  def = Mmu
    { _bios = V.fromList [
    0x31, 0xFE, 0xFF, 0xAF, 0x21, 0xFF, 0x9F, 0x32, 0xCB, 0x7C, 0x20, 0xFB, 0x21, 0x26, 0xFF, 0x0E,
    0x11, 0x3E, 0x80, 0x32, 0xE2, 0x0C, 0x3E, 0xF3, 0xE2, 0x32, 0x3E, 0x77, 0x77, 0x3E, 0xFC, 0xE0,
    0x47, 0x11, 0x04, 0x01, 0x21, 0x10, 0x80, 0x1A, 0xCD, 0x95, 0x00, 0xCD, 0x96, 0x00, 0x13, 0x7B,
    0xFE, 0x34, 0x20, 0xF3, 0x11, 0xD8, 0x00, 0x06, 0x08, 0x1A, 0x13, 0x22, 0x23, 0x05, 0x20, 0xF9,
    0x3E, 0x19, 0xEA, 0x10, 0x99, 0x21, 0x2F, 0x99, 0x0E, 0x0C, 0x3D, 0x28, 0x08, 0x32, 0x0D, 0x20,
    0xF9, 0x2E, 0x0F, 0x18, 0xF3, 0x67, 0x3E, 0x64, 0x57, 0xE0, 0x42, 0x3E, 0x91, 0xE0, 0x40, 0x04,
    0x1E, 0x02, 0x0E, 0x0C, 0xF0, 0x44, 0xFE, 0x90, 0x20, 0xFA, 0x0D, 0x20, 0xF7, 0x1D, 0x20, 0xF2,
    0x0E, 0x13, 0x24, 0x7C, 0x1E, 0x83, 0xFE, 0x62, 0x28, 0x06, 0x1E, 0xC1, 0xFE, 0x64, 0x20, 0x06,
    0x7B, 0xE2, 0x0C, 0x3E, 0x87, 0xF2, 0xF0, 0x42, 0x90, 0xE0, 0x42, 0x15, 0x20, 0xD2, 0x05, 0x20,
    0x4F, 0x16, 0x20, 0x18, 0xCB, 0x4F, 0x06, 0x04, 0xC5, 0xCB, 0x11, 0x17, 0xC1, 0xCB, 0x11, 0x17,
    0x05, 0x20, 0xF5, 0x22, 0x23, 0x22, 0x23, 0xC9, 0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B,
    0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D, 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E,
    0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99, 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC,
    0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E, 0x3c, 0x42, 0xB9, 0xA5, 0xB9, 0xA5, 0x42, 0x4C,
    0x21, 0x04, 0x01, 0x11, 0xA8, 0x00, 0x1A, 0x13, 0xBE, 0x20, 0xFE, 0x23, 0x7D, 0xFE, 0x34, 0x20,
    0xF5, 0x06, 0x19, 0x78, 0x86, 0x23, 0x05, 0x20, 0xFB, 0x86, 0x20, 0xFE, 0x3E, 0x01, 0xE0, 0x50
  ]
    , _rom   = emptyMem [0x0000..0x3FFF]
    , _srom  = emptyMem [0x4000..0x7FFF]
    , _eram  = emptyMem [0xA000..0xBFFF]
    , _wram  = emptyMem [0xC000..0xCFFF]
    , _swram = emptyMem [0xD000..0xDFFF]
    }

-- | Replace the each element of the list by a null byte
emptyMem :: [a] -> V.Vector Word8
emptyMem = V.fromList . map (const 0)

-- | The Virtual Machine
data Vm = Vm { _vmCpu :: Cpu, _vmMmu :: Mmu, _vmCartridge :: Maybe CartridgeDesc}
        deriving (Show, Eq)

instance Default Vm where
  def = Vm { _vmCpu = def, _vmMmu = def, _vmCartridge = Nothing }

type VmS = State Vm

data Instruction = Instruction
  { _iClock :: !Clock
  , _iVmS   :: !(VmS ())
  }

-- | Zero Flag
fZ :: Word8
fZ = shiftL 1 7
-- | Substraction flag
fN :: Word8
fN = shiftL 1 6
-- | Half-carry flag
fH :: Word8
fH = shiftL 1 5
-- | Carry flag
fC :: Word8
fC = shiftL 1 4

-- | Game boy color flag
data CGBFlag = CGBOnly | CGBCompat | CGBOff
data SCGBFlag = SCGBOn | SCGBOff
data CartridgeType = Cartridge
  { mbcType :: MBCType
  , ram :: Bool
  , battery :: Bool
  , timer :: Bool
  , rumble :: Bool}
  | PocketCamera
  | Tama5
  | HuC3
  | HuC1
  deriving (Show, Eq)

instance Default CartridgeType where
  def = Cartridge
    { mbcType = ROM
    , ram = False
    , battery = False
    , timer = False
    , rumble = False
    }

data MBCType = ROM
             | MBC1
             | MBC2
             | MBC3
             | MBC4
             | MBC5
             | MMM01
             deriving (Show, Eq)

-- | Describe a cartridge
data CartridgeDesc = CartridgeDesc
  { cdTitle        :: String
  , cdManufacturer :: String
  , cdType         :: CartridgeType
  } deriving (Show, Eq)


makeClassy ''Mmu
makeClassy ''Vm
makeClassy ''Instruction
makeClassy ''Cpu
makeClassy ''Registers
makeClassy ''Clock

instance HasCpu Vm where cpu = vmCpu
instance HasMmu Vm where mmu = vmMmu
instance HasRegisters Cpu where registers = cpuRegisters
instance HasRegisters Vm where registers = vmCpu . cpuRegisters
instance HasClock Cpu where clock = cpuClock
instance HasClock Vm where clock = vmCpu . cpuClock
instance HasClock Instruction where clock = iClock
