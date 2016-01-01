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
import           Data.List (unfoldr, foldl')

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

-- | Combine binary values to create a word8
boolCombine :: [Bool] -> Word8
boolCombine = foldl fct 0
  where
    fct stack bool = (boolToWord8 bool) .|. (stack `shiftL` 1)

boolUncombine :: Word8 -> [Bool]
boolUncombine v = wordToBool `fmap` unfoldr getBit (v, 0)
  where
    getBit (v, idx) = case idx of
      8 -> Nothing
      _ -> Just (v `shiftR` 7, (v `shiftL` 1, idx + 1))

-- | Convert a 'Bool' to a 'Word8' with the rule :
--   True  = 1
--   False = 0
boolToWord8 :: Bool -> Word8
boolToWord8 False = 0
boolToWord8 True  = 1

wordToBool :: Word8 -> Bool
wordToBool = (/= 0)

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

-- | Grey color if the GB,
--   0 is 'WHITE' and 3 is 'BLACK'.
data GreyScale = WHITE
               | LIGHTGREY
               | DARKGREY
               | BLACK
               deriving (Show, Eq, Ord, Enum)

gsToNum :: (Integral a) => GreyScale -> a
gsToNum WHITE = 0
gsToNum LIGHTGREY = 1
gsToNum DARKGREY = 2
gsToNum BLACK = 3

numToGS :: (Integral a) => a -> GreyScale
numToGS 0 = WHITE
numToGS 1 = LIGHTGREY
numToGS 2 = DARKGREY
numToGS _ = BLACK

data Color = RED | GREEN | BLUE

-- | Give the offset in a 'Word8' 'Vector' to access the 'Color'
colorOffset :: Integral a => Color -> a
colorOffset RED   = 0
colorOffset GREEN = 1
colorOffset BLUE  = 2


data GpuMode = HorizontalBlank
               -- ^ Horizontable blank mode. Both
               --   OAM and VRAM are accessible.
             | ScanlineOAM
               -- ^ First part of the scanline mode.
               --   The OAM is used and unaccessible from
               --   the CPU.
             | ScanlineVRAM
               -- ^ Second part of the scanline mode.
               --   Both OAM and VRAM are used by the GPU
               --   and are unaccessible from the CPU.
             | VerticalBlank
               -- ^ Vertical blank mode. Both
               --   OAM and VRAM are accessible.
             deriving (Show, Eq)

-- | The LCDC register. It's default value is 0x91.
data LCDCf = LCDCf { _lcdcDisplay        :: Bool
                     -- ^ Bit 7 - LCD Control Operation *
                     --     0: Stop completely (no picture on screen)
                     --     1: operation
                   , _lcdcTileMap        :: Bool
                     -- ^ Bit 6 - Window Tile Map Display Select
                     --     0: $9800-$9BFF
                     --     1: $9C00-$9FFF
                   , _lcdcWindow         :: Bool
                     -- ^ Bit 5 - Window Display
                     --     0: off
                     --     1: on
                   , _lcdcTileSetSelect  :: Bool
                     -- ^ Bit 4 - BG & Window Tile Data Select
                     --     0: $8800-$97FF
                     --     1: $8000-$8FFF <- Same area as OBJ
                   , _lcdcTileMapSelect  :: Bool
                     -- ^ Bit 3 - BG Tile Map Display Select
                     --     0: $9800-$9BFF
                     --     1: $9C00-$9FFF
                   , _lcdcSpriteSize     :: Bool
                     -- ^ Bit 2 - OBJ (Sprite) Size
                     --     0: 8*8
                     --     1: 8*16 (width*height)
                   , _lcdcSpriteDisplay  :: Bool
                     -- ^ Bit 1 - OBJ (Sprite) Display
                     --     0: off
                     --     1: on
                   , _lcdcWindowDisplay  :: Bool
                     -- ^ Bit 0 - BG & Window Display
                     --     0: off
                     --     1: on
                   } deriving (Show, Eq)

instance Default LCDCf where
   def = word8ToLCDC 0x91

lcdcfList = [ _lcdcWindowDisplay
             , _lcdcSpriteDisplay
             , _lcdcWindow
             , _lcdcTileSetSelect
             , _lcdcTileMapSelect
             , _lcdcSpriteSize
             , _lcdcSpriteDisplay
             , _lcdcWindowDisplay
             ]

lcdcToWord8 :: LCDCf -> Word8
lcdcToWord8 v = boolCombine $ lcdcfList <*> [v]

word8ToLCDC :: Word8 -> LCDCf
word8ToLCDC v = LCDCf b0 b1 b2 b3 b4 b5 b6 b7
  where
    [b0, b1, b2, b3, b4, b5, b6, b7] = boolUncombine v

-- | Represent the memory, registers and flags
data Gpu = Gpu { _vram         :: !(V.Vector Word8)
                 -- ^ Vram
               , _gpuMode      :: !GpuMode
                 -- ^ Current mode of the GPU
               , _gpuClock     :: !Word
                 -- ^ Clock used to witch modes
               , _gpuLine      :: !Word16
                 -- ^ Number of the current line
               , _renderingMem :: !(V.Vector Word8)
                 -- ^ Memory used for rendering the screen
               , _scx          :: !Word8
                 -- ^ Scroll X register
               , _scy          :: !Word8
                 -- ^ Scroll Y register
               , _bgPalette    :: !Word8
                 -- ^ Background Palette
               , _lcdcf         :: !LCDCf
                 -- ^ LCDC flag
               } deriving (Show, Eq)

-- | Compute the location of the pixel ('x','y') of color 'c'
--   in the 'renderingMem' field of 'Gpu'.
rendMemLoc :: Int -> Int -> Color -> Int
rendMemLoc x y c = ((x + y * 160) * 3 + (colorOffset c))

instance Default Gpu where
  def = Gpu
    { _vram = emptyMem [0x8000..0x9FFF]
    , _gpuMode = HorizontalBlank
    , _gpuClock = 0
    , _gpuLine = 0
    , _renderingMem = emptyMem [0..144*166*3]
    , _scx = 0
    , _scy = 0
    , _bgPalette = 0 -- TODO
    , _lcdcf = def
    }

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
  , _oam   :: !(V.Vector Word8)
  -- ^ FE00-FE9F   Sprite Attribute Table (OAM)
  , _hram  :: !(V.Vector Word8)
  -- ^ FF80-FFFE   High RAM (HRAM)
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
    , _oam   = emptyMem [0xFE00..0xFE9F]
    , _hram  = emptyMem [0xFF80..0xFFFE]
    , _ier = 0x00
    , _biosEnabled = True
    , _mmuGpu = def
    }

-- | Replace the each element of the list by a null byte
emptyMem :: [Integer] -> V.Vector Word8
emptyMem = V.fromList . map (const 0)

-- | The Virtual Machine
data Vm = Vm { _vmCpu :: !Cpu, _vmMmu :: Mmu, _cartridge :: !CartridgeDesc}
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
makeClassy ''LCDCf

instance HasCpu Vm where cpu = vmCpu
instance HasMmu Vm where mmu = vmMmu
instance HasRegisters Vm where registers = vmCpu . cpuRegisters
instance HasClock Vm where clock = vmCpu . cpuClock
instance HasRegisters Cpu where registers = cpuRegisters
instance HasClock Cpu where clock = cpuClock
instance HasGpu Mmu where gpu = mmuGpu
instance HasGpu Vm where gpu = vmMmu . mmuGpu
