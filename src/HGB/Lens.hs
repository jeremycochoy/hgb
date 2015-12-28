{-# LANGUAGE Rank2Types      #-}

module HGB.Lens where

import           Control.Lens
import           HGB.Types
import           HGB.MMU
import           Control.Applicative
import           Data.Word (Word8, Word16)
import           Data.Bits
import           Data.Bits.Lens

-- | Z flag saw as a lens
lZf :: HasRegisters t => Lens' t Bool
lZf = lFlag fZ

-- | Z flag saw as a lens
lHf :: HasRegisters t => Lens' t Bool
lHf = lFlag fH

-- | N flag saw as a lens
lNf :: HasRegisters t => Lens' t Bool
lNf = lFlag fN

-- | N flag saw as a lens
lCf :: HasRegisters t => Lens' t Bool
lCf = lFlag fC

-- | not Z flag saw as a lens
lNZf :: HasRegisters t => Lens' t Bool
lNZf = lNFlag lZf

-- | not N flag saw as a lens
lNCf :: HasRegisters t => Lens' t Bool
lNCf = lNFlag lCf

-- | Flag saw as a lens
lFlag :: HasRegisters t => Word8 -> Lens' t Bool
lFlag flag f' reg = writeFlag <$> f' readFlag
  where
    readFlag = 0 /= (reg ^. f) .&. flag
    writeFlag val = case val of
      False -> f .&.~ (complement flag) $ reg
      True -> f .|.~ flag $ reg

-- | Flag saw as a lens (with value negated by "not")
lNFlag :: HasRegisters t => Lens' Registers Bool -> Lens' t Bool
lNFlag fl f' reg = (set (registers . fl) ?? reg) . not <$>
                  f' (not $ reg ^. registers . fl)

-- | (BC) saw as a lens
lBCm :: Lens' Vm Word8
lBCm = lRRm b c

-- | (DE) saw as a lens
lDEm :: Lens' Vm Word8
lDEm = lRRm d e

-- | (HL) saw as a lens
lHLm :: Lens' Vm Word8
lHLm = lRRm h l

-- | BC saw as a lens
lBC :: HasRegisters t => Lens' t Word16
lBC = lRR b c

-- | DE saw as a lens
lDE :: HasRegisters t => Lens' t Word16
lDE = lRR d e

-- | HL saw as a lens
lHL :: HasRegisters t => Lens' t Word16
lHL = lRR h l

-- | AF saw as a lens
lAF :: HasRegisters t => Lens' t Word16
lAF f' reg = (writeRR a f reg . (.&. 0xFFF0)) <$> f' (0xFFF0 .&. readRR a f reg)

-- | (RR) saw as a lens
lRRm :: ALens' Registers Word8 -> ALens' Registers Word8 -> Lens' Vm Word8
lRRm h' l' f' vm' = (writeRRm (cloneLens h') (cloneLens l') vm') <$>
                  f' (readRRm (cloneLens h') (cloneLens l') vm')

-- | RR (where R means a register) saw as a lens
lRR :: HasRegisters r => ALens' Registers Word8 -> ALens' Registers Word8 -> Lens' r Word16
lRR h' l' f' reg = writeRR (cloneLens h') (cloneLens l') reg <$>
                  f' (readRR (cloneLens h') (cloneLens l') reg)

-- | Compute r:r as a 16 bits addr
readRR :: HasRegisters r => Getting Word8 Registers Word8 -> Getting Word8 Registers Word8 -> r ->  Word16
readRR h' l' reg = wCombine (reg ^. registers . h') (reg ^. registers . l')

-- | Compute r:r as a 16 bits addr
writeRR :: (HasRegisters r) => ASetter' Registers Word8 -> ASetter' Registers Word8 -> r -> Word16 -> r
writeRR h' l' reg v = registers . h' .~ (fromIntegral $ shiftR v 8) $
                      registers . l' .~ (fromIntegral v) $ reg

-- | Read from (RR) : The value at location r:r
readRRm :: Getting Word8 Registers Word8 -> Getting Word8 Registers Word8 -> Vm -> Word8
readRRm h' l' vm' = rb idx (vm' ^. mmu)
  where idx = readRR h' l' (vm' ^. registers)

-- | Write on (RR) : Write at location r:r
writeRRm :: Getting Word8 Registers Word8 -> Getting Word8 Registers Word8 -> Vm -> Word8 -> Vm
writeRRm h' l' vm' v = mmu %~ (wb idx v) $ vm'
  where idx = readRR h' l' (vm' ^. registers)

-- | Read from (RR) : The value at location r:r
lSPm16 :: Lens' Vm Word16
lSPm16 f' vm' = writeSPm <$> f' readSPm
  where
    readSPm = rw (vm' ^. sp) (vm' ^. mmu)
    writeSPm v = mmu %~ (ww (vm' ^. sp) v) $ vm'

-- | Read from (a16) where a16 is the address given.
la16 :: Word16 -> Lens' Vm Word8
la16 addr f' vm' = (\v -> mmu %~ (wb addr v) $ vm') <$> f' (rb addr (vm' ^. mmu))

-- | Read from (0xFF00 + addrL) where addrL is the address given.
la8 :: Word8 -> Lens' Vm Word8
la8 addrL f' vm' = la16 (0xFF00 + fromIntegral addrL) f' vm'
