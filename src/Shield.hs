import SFML.Window
import SFML.Graphics
import SFML.Graphics.Color

import HGB.Types
import HGB.Cartridge
import HGB.CPU
import Control.Lens
import Control.Monad.State
import System.Environment (getArgs)
import Data.Default
import qualified Data.Vector.Unboxed as V2
import qualified Data.Vector.Storable as V
import Data.Word
import Foreign.ForeignPtr

data ShieldState = ShieldState
                   { ssKey    :: Maybe KeyCode
                   , ssScale  :: Int
                   , ssVm     :: Vm
                   , drawArea :: V.Vector Word8
                   } deriving Show

instance Default ShieldState where
  def = ShieldState
        { ssKey = Nothing
        , ssScale = 4
        , ssVm = def
        , drawArea = V.replicate (gameboyScreenWidth * gameboyScreenHeight * 4) 255
        }


main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage : hgb file.gb"
    _  -> do
      vm' <- loadRom . head $ args
      case vm' of
        Left msg   -> putStrLn msg
        Right vm'' -> do
          launchShield $ def { ssVm = vm'' }

-- Todo : no resizing
launchShield :: ShieldState -> IO ()
launchShield ss = do
  window <- createRenderWindow (VideoMode sWidth sHeight 32) "HGB - Devel Version" [SFDefaultStyle] Nothing
  clearRenderWindow window SFML.Graphics.Color.white
  Right sprite <- createSprite
  Right texture <- createTexture gameboyScreenWidth gameboyScreenHeight
  setTexture sprite texture True
  loop ss window texture sprite
  destroy window
    where
      sWidth = gameboyScreenWidth
      sHeight = gameboyScreenHeight

loop :: ShieldState -> RenderWindow -> Texture -> Sprite -> IO ()
loop ss window texture sprite = do
  maybeSs <- processEvent ss window
  case maybeSs of
    Nothing                 -> return ()
    Just newSs -> do
      let ss' = updateMemory newSs {ssVm = execState execFrame (ssVm newSs)}
--      putStrLn . show $ ((ssVm ss) ^. registers, (ssVm ss) ^. t)
--      putStrLn . show $ drawArea ss'
      setWindowSize window (windowSize ss')
      renderShield ss' window texture sprite
      loop ss' window texture sprite
  where
    windowSize ss = Vec2u (fromIntegral $ ssScale ss * gameboyScreenWidth)
                          (fromIntegral $ ssScale ss * gameboyScreenHeight)

-- | Update the pixels in Drawing Area from the VM
updateMemory :: ShieldState -> ShieldState
updateMemory ss = ss { drawArea = up $ (ssVm ss) ^. renderingMem }
  where
    up input = V.fromList . fst $ V2.foldr' f ([], 0) input
    f v (list, idx) = case idx `mod` 3 of
                   0 -> (v : 255 : list, idx + 1)
                   _ -> (v : list, idx + 1)

renderShield :: ShieldState -> RenderWindow -> Texture -> Sprite -> IO ()
renderShield ss window texture sprite = do
  setScale sprite $ Vec2f 1 1
  drawSprite window sprite Nothing
  withForeignPtr ptr updateTex
  display window
  where
    (ptr, len) = V.unsafeToForeignPtr0 (drawArea ss)
    updateTex ptr = updateTextureFromPixels texture ptr 160 144 0 0

processEvent :: ShieldState -> RenderWindow -> IO (Maybe ShieldState)
processEvent ss window = do
  event <- pollEvent window
  case event of
    Just SFEvtClosed         -> return Nothing
    Just e@SFEvtKeyReleased{} -> case code e of
      KeyUp     -> release jpUp
      KeyDown   -> release jpDown
      KeyLeft   -> release jpLeft
      KeyRight  -> release jpRight
      KeyF      -> release jpA
      KeyD      -> release jpB
      KeyE      -> release jpSelect
      KeyR      -> release jpSelect
      _         -> processEvent ss window
    Just e@SFEvtKeyPressed{} -> case code e of
      -- GB Key
      KeyUp     -> press jpUp
      KeyDown   -> press jpDown
      KeyLeft   -> press jpLeft
      KeyRight  -> press jpRight
      KeyF      -> press jpA
      KeyD      -> press jpB
      KeyE      -> press jpSelect
      KeyR      -> press jpSelect
      -- Emulator's Key
      KeyEscape -> return Nothing
      KeyH -> do
        let f x = x `mod` 4 + 1
        putStrLn . show $ ssScale ss
        putStrLn . show $ f (ssScale ss)
        return . Just $ ss { ssScale = f (ssScale ss) }
      -- Debuging keys
      KeyP     -> do
        putStrLn . show $ (vm' ^. registers, vm' ^. cpuClock)
        return $ Just ss
      KeyV     -> do
        putStr . show $ vm' ^. vram
        return $ Just ss
      _    -> processEvent ss window
    Nothing                  -> return . Just $ ss { ssKey = Nothing }
    _                        -> processEvent ss window
  where
    release keyLens = return . Just $ ss { ssVm = keyLens .~ False $ (ssVm ss) }
    press   keyLens = return . Just $ ss { ssVm = keyLens .~ False $ (ssVm ss) }
    vm'             = ssVm ss
