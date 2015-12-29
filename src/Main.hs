module Main where

import HGB.Types
import HGB.CPU
import HGB.MMU
import HGB.Cartridge
import Text.Groom
import System.Environment (getArgs)
import Control.Lens ((^.))
import Control.Monad.State (execState)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word8)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage : hgb file.gb"
    _  -> do
      vm' <- loadRom . head $ args
      case vm' of
        Left msg   -> putStrLn msg
        Right vm'' -> do
          putStrLn . groom $ vm'' ^. cartridge
          putStrLn . groom $ vm'' ^. cpu
          runStep' vm''

runStep :: Vm -> IO ()
runStep oldVm = do
  -- Wait from user input
  getLine
  -- Compute next step
  newVM <- return . execState exec $ oldVm
  -- Display CPU
  putStrLn . groom $ newVM ^. cpu
  -- Loop
  runStep newVM

runStep' :: Vm -> IO ()
runStep' oldVm = do
  newVM <- return . execState exec $ oldVm
--  putStr . show $ newVM ^. registers
--  putStr "\r"
  newVM `seq` runStep' newVM

runStep'' :: Int -> Vm -> IO ()
runStep'' i oldVm = do
  newVM <- return . execState exec $ oldVm
  case i of
    800 -> do
      debugDisp (newVM ^. vram)
      newVM `seq` runStep'' 0 newVM
    _ ->  newVM `seq` runStep'' (i+1) newVM

-- | Display the memory (debug purpose)
debugDisp :: V.Vector Word8 -> IO ()
debugDisp vec = do
  --showLine 0
  putStrLn . groom $ vec
  putStrLn ""
  where
    v = V.slice 0x800 (0x9BFF - 0x9800 + 1) $ vec
    showLine 32 = return ()
    showLine i = showChar i 0 >> putStrLn "" >> showLine (i+1)
    showChar _ 32 = return ()
    showChar i j = (putStr . show $ v V.! (i*32 + j)) >> showChar i (j + 1)
