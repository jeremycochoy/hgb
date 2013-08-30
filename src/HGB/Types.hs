{-# LANGUAGE TemplateHaskell #-}

module HGB.Types where

import           Data.Word (Word8(..), Word16(..), Word(..))
import           Control.Lens
import           Data.Default
import qualified Data.Vector.Unboxed as V
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

-- | The MMU (memory)
data Mmu = Mmu
  { _bios :: !(V.Vector Word8)
  , _rom  :: !(V.Vector Word8)
  , _wram :: !(V.Vector Word8)
  , _eram :: !(V.Vector Word8)
  , _zram :: !(V.Vector Word8)
  } deriving (Show, Eq)


instance Default Mmu where
  def = Mmu
    { _bios = V.fromList [0]
    , _rom  = V.fromList [0]
    , _wram = V.fromList [0]
    , _eram = V.fromList [0]
    , _zram = V.fromList [0]
    }

-- | The Virtual Machine
data Vm = Vm { _vmCpu :: Cpu, _vmMmu :: Mmu }
          deriving (Show, Eq)


instance Default Vm where
  def = Vm { _vmCpu = def, _vmMmu = def }

type VmS = State Vm

data Instruction = Instruction
  { _iClock :: !Clock
  , _iVmS   :: !(VmS ())
  }

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
