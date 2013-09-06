module HGB.Cartridge
  ( loadRom
  , bytestringToVector
  , getCartridgeType
  ) where

import           Data.Word (Word8)
import           HGB.Types
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as V
import           Control.Error.Safe
import           Data.List (unfoldr)
import           Control.Lens ((.~))

getCartridgeType :: Word8 -> Maybe CartridgeType
getCartridgeType 0x00 = Just $ def
getCartridgeType 0x01 = Just $ def {mbcType = MBC1}
getCartridgeType 0x02 = Just $ def {mbcType = MBC1, ram = True}
getCartridgeType 0x03 = Just $ def {mbcType = MBC1, ram = True, battery = True}
getCartridgeType 0x05 = Just $ def {mbcType = MBC2}
getCartridgeType 0x06 = Just $ def {mbcType = MBC2, battery = True}
getCartridgeType 0x08 = Just $ def {mbcType = ROM, ram = True}
getCartridgeType 0x09 = Just $ def {mbcType = ROM, ram = True, battery = True}
getCartridgeType 0x0B = Just $ def {mbcType = MMM01}
getCartridgeType 0x0C = Just $ def {mbcType = MMM01, ram = True}
getCartridgeType 0x0D = Just $ def {mbcType = MMM01, ram = True, battery = True}
getCartridgeType 0x0F = Just $ def {mbcType = MBC3, timer = True, battery = True}
getCartridgeType 0x10 = Just $ def {mbcType = MBC3, timer = True, ram = True, battery = True}
getCartridgeType 0x11 = Just $ def {mbcType = MBC3}
getCartridgeType 0x12 = Just $ def {mbcType = MBC3, ram = True}
getCartridgeType 0x13 = Just $ def {mbcType = MBC3, ram = True, battery = True}
getCartridgeType 0x15 = Just $ def {mbcType = MBC4}
getCartridgeType 0x16 = Just $ def {mbcType = MBC4, ram = True}
getCartridgeType 0x17 = Just $ def {mbcType = MBC4, ram = True, battery = True}
getCartridgeType 0x19 = Just $ def {mbcType = MBC5}
getCartridgeType 0x1A = Just $ def {mbcType = MBC5, ram = True}
getCartridgeType 0x1B = Just $ def {mbcType = MBC5, ram = True, battery = True}
getCartridgeType 0x1C = Just $ def {mbcType = MBC5, rumble = True}
getCartridgeType 0x1D = Just $ def {mbcType = MBC5, rumble = True, ram = True}
getCartridgeType 0x1E = Just $ def {mbcType = MBC5, rumble = True, ram = True, battery = True}
getCartridgeType 0xFC = Just $ PocketCamera
getCartridgeType 0xFD = Just $ Tama5
getCartridgeType 0xFE = Just $ HuC3
getCartridgeType 0xFF = Just $ HuC1
getCartridgeType _ = Nothing

bytestringToVector :: B.ByteString -> V.Vector Word8
bytestringToVector = V.unfoldr (B.uncons)

mmuFromRomFile :: FilePath -> IO (Either String Mmu)
mmuFromRomFile path = do
  vec <- bytestringToVector `fmap` B.readFile path
  return $ case V.length vec of
    0x8000 -> Right $ def
      { _rom  = V.slice 0x0000 0x4000 vec
      , _srom = V.slice 0x4000 0x4000 vec
      }
    _      -> Left "Unrocognisez length"

describeCartridge :: Mmu -> Either String CartridgeDesc
describeCartridge mmu' = do
  t' <- justErr "Unknown cartridge type" . getCartridgeType $ cart V.! 0x147
  Right $ CartridgeDesc title' man t'
  where
    cart = (_rom mmu')
    titleAt i = cart V.! (0x0134 + i)
    manAt   i = cart V.! (0x013F + i)
    getAt at max' i = case i < max' && at i /= 0 of
      False -> Nothing
      True  -> Just $ (toEnum . fromEnum $ at i, i + 1)
    getTitle = getAt titleAt 11
    getMan   = getAt manAt   4
    title' = unfoldr getTitle 0
    man   = unfoldr getMan   0

loadRom :: FilePath -> IO (Either String Vm)
loadRom path = do
  mmuE <- mmuFromRomFile path
  return $ do
    mmu' <- mmuE
    cartridge' <- describeCartridge mmu'
    Right . (mmu .~ mmu') . (cartridge .~ cartridge') $ def
