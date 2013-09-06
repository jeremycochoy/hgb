module Main where

import HGB.Types
import HGB.CPU
import HGB.MMU
import HGB.Cartridge
import Text.Groom
import System.Environment (getArgs)
import Control.Lens ((^.))
import Control.Monad.State (execState)

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
  putStr . show $ newVM ^. registers
  putStr "\r"
  newVM `seq` runStep' newVM
