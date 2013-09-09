{-# LANGUAGE TemplateHaskell #-}

module HGB.Types
    ( module HGB.Types
    ) where

import           Data.Word (Word8, Word16, Word)
import           Control.Lens
import           Data.Default as HGB.Types
import qualified Data.Vector.Unboxed as V
-- We re-export the operator !
import qualified Data.Vector.Unboxed as HGB.Types ((!))
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

-- | Combine the two input bytes h and l into a Word16 as h:l.
wCombine :: Word8 -> Word8 -> Word16
wCombine h l = (shiftL (fromIntegral h) 8) .|. (fromIntegral l)

-- | Break the higher and lower part of the input as (High, Low).
wUncombine :: Word16 -> (Word8, Word8)
wUncombine hl = (fromIntegral $ shiftR hl 8, fromIntegral hl)

-- | Binary swap the 4 lower bits of the input with the 4 higher bits.
swap :: Word8 -> Word8
swap v = (shiftR v 4) .|. (shiftL v 4)

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

data InterruptState = IEnabled | IDisabled | INextInstD | INextInstE
                    deriving (Eq, Show)

-- | The Z80 CPU
data Cpu = Cpu { _cpuRegisters :: !Registers
               , _cpuClock :: !Clock
               , _interrupt :: !InterruptState
               } deriving (Show, Eq)

instance Default Cpu where
  def = Cpu
    { _cpuRegisters = def
    , _cpuClock = def
    , _interrupt = IDisabled
    }

data Gpu = Gpu { _vram :: !(V.Vector Word8) } deriving (Show, Eq)

instance Default Gpu where
  def = Gpu { _vram = emptyMem [0x8000..0x9FFF] }

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
  , _ier   :: Word8
  -- ^ FFFF   Interrupt Enable Register
  , _biosEnabled :: Bool
  -- ^ When true, reading below 0x100 access the bios.
  --   Otherwise, it reads from the _rom field.
  , _mmuGpu :: Gpu
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
    , _ier = 0x00
    , _biosEnabled = True
    , _mmuGpu = def
    }

-- | Replace the each element of the list by a null byte
emptyMem :: [Integer] -> V.Vector Word8
emptyMem = V.fromList . map (const 0)

-- | The Virtual Machine
data Vm = Vm { _vmCpu :: Cpu, _vmMmu :: Mmu, _cartridge :: CartridgeDesc}
        deriving (Show, Eq)

instance Default Vm where
  def = Vm { _vmCpu = def, _vmMmu = def, _cartridge = def }

type VmS = State Vm

-- | An Instruction is the work associated to an opcode
--   It should "return" the time it took to complete.
type Instruction = VmS Clock

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
  { _title         :: String
  , _manufacturer  :: String
  , _cartridgeType :: CartridgeType
  } deriving (Show, Eq)

instance Default CartridgeDesc where
  def = CartridgeDesc {_title = "", _manufacturer = "", _cartridgeType = def}

makeClassy ''Mmu
makeClassy ''Vm
makeClassy ''Cpu
makeClassy ''Gpu
makeClassy ''Registers
makeClassy ''Clock
makeClassy ''CartridgeDesc

instance HasCpu Vm where cpu = vmCpu
instance HasMmu Vm where mmu = vmMmu
instance HasRegisters Vm where registers = vmCpu . cpuRegisters
instance HasClock Vm where clock = vmCpu . cpuClock
instance HasRegisters Cpu where registers = cpuRegisters
instance HasClock Cpu where clock = cpuClock
instance HasGpu Mmu where gpu = mmuGpu
instance HasGpu Vm where gpu = vmMmu . mmuGpu


