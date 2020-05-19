{-# LANGUAGE ScopedTypeVariables #-}
module Mix where

import MixInternal

-- IDEA: Purely functional programming means easy to keep old states b/c of pointer machine.
--       Record every state and allow reversing etc?

stepMachine :: MixMachine -> Error MixMachine
stepMachine m = do
    instr <- readInstruction =<< (getMemory' m <$> checkAddress m (ip m))
    let f = iMode instr
    case iOpc instr of
      NOP   -> incIp m
      LD r  -> incIp =<< setRegister m r . getMemory m f <$> getAddress m instr
      LDN r -> incIp =<< setRegister m r . wNegate . getMemory m f <$> getAddress m instr
      ST r  -> incIp =<< setMemory m f (getRegister m r) <$> getAddress m instr
      STZ   -> incIp =<< setMemory m f zeroWord          <$> getAddress m instr
      ADD   -> do
        v <- getMemory m f <$> getAddress m instr
        let (sum, overflow) = plus (base m) v $ getRegister m RA
        incIp $ setRegister (m { overflow=overflow }) RA sum
      SUB   -> do
        v <- wNegate . getMemory m f <$> getAddress m instr
        let (sum, overflow) = plus (base m) v $ getRegister m RA
        incIp $ setRegister (m { overflow=overflow }) RA sum
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
