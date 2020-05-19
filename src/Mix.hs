{-# LANGUAGE ScopedTypeVariables #-}
module Mix (stepMachine) where

import Data.Int (Int8, Int64)
import Debug.Trace

import MixInternal

-- IDEA: Purely functional programming means easy to keep old states b/c of pointer machine.
--       Record every state and allow reversing etc?

stepMachine :: MixMachine -> Error MixMachine
stepMachine m = do
    instr <- readInstruction =<< (getMemory m <$> checkAddress m (ip m))
    case iOpc instr of
      NOP   -> incIp m
      LD r  -> incIp =<< setRegister m r . getMemory m <$> getAddress m instr
      LDN r -> incIp =<< setRegister m r . wNegate . getMemory m <$> getAddress m instr
      ST r  -> incIp =<< setMemory m (getRegister m r) <$> getAddress m instr
      STZ   -> incIp =<< setMemory m zeroWord          <$> getAddress m instr
      ADD   -> undefined
      SUB   -> undefined
      MUL   -> undefined
      DIV   -> undefined
      NCH   -> undefined
      SHIFT -> undefined
      MOVE  -> undefined
      IOC   -> undefined
      JRED  -> undefined
      JUMP  -> undefined
      JR r  -> undefined
      IDE r -> undefined
      CMP r -> undefined
      JBUS  -> undefined
      IN    -> undefined
      OUT   -> undefined
    where contents = getMemory 
          getAddress :: MixMachine -> Instruction -> Error CheckedAddress
          getAddress m i = checkAddress m $ fromWord a
              where offset = maybe zeroWord (getRegister m) $ iIndex i
                    a = fst $ plus (base m) (iAddr i) offset
          incIp m = Right m { ip=fst $ asInt' (+1) (ip m) }
          asInt' = asInt (base m)

                    

someFunc :: IO ()
someFunc = putStrLn "someFunc"
