import SFML.Window
import SFML.Graphics
import SFML.Graphics.Color

import HGB.Types
import Data.Default
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.ForeignPtr

data ShieldState = ShieldState
                   { ssKey :: Maybe KeyCode
                   , ssScale :: Int
                   } deriving Show

instance Default ShieldState where
  def = ShieldState
        { ssKey = Nothing
        , ssScale = 2
        }

mem = V.replicate (160*144*4) 155 :: V.Vector Word8
-- Todo : no resizing
main :: IO ()
main = do
  window <- createRenderWindow (VideoMode sWidth sHeight 32) "HGB - Devel Version" [SFTitlebar] Nothing
  clearRenderWindow window SFML.Graphics.Color.white
  Right sprite <- createSprite
  Right texture <- createTexture sWidth sHeight
  setTexture sprite texture True
  loop def window texture sprite
  destroy window
    where
      sWidth = gameboyScreenWidth * 4
      sHeight = gameboyScreenHeight * 4

loop :: ShieldState -> RenderWindow -> Texture -> Sprite -> IO ()
loop ss window texture sprite = do
  maybeSs <- processEvent ss window
  case maybeSs of
    Nothing                 -> return ()
    Just newSs -> do
      setWindowSize window (windowSize ss)
--      putStrLn . show $ newSs
      renderShield window texture sprite
      loop newSs window texture sprite
  where
    windowSize ss = Vec2u (fromIntegral $ ssScale ss * gameboyScreenWidth)
                       (fromIntegral $ ssScale ss * gameboyScreenHeight)

renderShield :: RenderWindow -> Texture -> Sprite -> IO ()
renderShield window texture sprite = do
  setScale sprite (Vec2f 4 4)
  drawSprite window sprite Nothing
  withForeignPtr ptr updateTex
  display window
  where
    (ptr, len) = V.unsafeToForeignPtr0 mem
    updateTex ptr = updateTextureFromPixels texture ptr 160 144 0 0

processEvent :: ShieldState -> RenderWindow -> IO (Maybe ShieldState)
processEvent ss window = do
  event <- pollEvent window
  case event of
    Just SFEvtClosed         -> return Nothing
    Just e@SFEvtKeyPressed{} -> case code e of
      KeyEscape -> return Nothing
      KeyH -> do
               let f x = x `mod` 4 + 1
               putStrLn . show $ ssScale ss
               putStrLn . show $ f (ssScale ss)
               return . Just $ ss { ssScale = f (ssScale ss) }
      _     -> processEvent ss window
    Nothing                  -> return . Just $ ss { ssKey = Nothing }
    _                        -> processEvent ss window
