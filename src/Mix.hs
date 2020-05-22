{-# LANGUAGE ScopedTypeVariables #-}
module Mix where

import MixInternal

-- IDEA: Purely functional programming means easy to keep old states b/c of pointer machine.
--       Record every state and allow reversing etc?

stepMachine :: MixMachine -> Error MixMachine
stepMachine m = do
    instr <- readInstruction =<< (getMemory' m <$> checkAddress m (ip m))
    let f = iMode instr
    let b = base m
    case iOpc instr of
      NOP   -> incIp m
      LD r  -> incIp =<< setRegister m r . getMemory m f <$> getAddress m instr
      LDN r -> incIp =<< setRegister m r . wNegate . getMemory m f <$> getAddress m instr
      ST r  -> incIp =<< setMemory m f (getRegister m r) <$> getAddress m instr
      STZ   -> incIp =<< setMemory m f zeroWord          <$> getAddress m instr

      ADD   -> do
        v <- getMemory m f <$> getAddress m instr
        let (sum, overflow) = plus b v $ getRegister m RA
        incIp $ setRegister (m { overflow=overflow }) RA sum
      SUB   -> do
        v <- wNegate . getMemory m f <$> getAddress m instr
        let (sum, overflow) = plus b v $ getRegister m RA
        incIp $ setRegister (m { overflow=overflow }) RA sum
      MUL   -> do
        -- TODO: The signs for RA,RX are always + when the value is 0.
        -- They should instead be the product of the signs for the inputs.
        v <- getMemory m f <$> getAddress m instr
        let (x, a) = times b v $ getRegister m RA
        incIp $ srmRegister RA a $ srmRegister RX x m
      DIV   -> do
        -- TODO: The signs for this are going to be fucked up around +/- 0.
        v <- toInt b . getMemory m f <$> getAddress m instr
        let (wa, wx) = (getRegister m RA, getRegister m RX)
            (a, x) = (toInt b wa, toInt b wx)
            ax = a * b^5 + (wsign wa * abs x)
            (q, r) = (ax `quot` v, ax `rem` v)
            (a', x') = (sign v * sign a * abs q, wsign wa * abs r)
        if abs a >= abs v
          then incIp $ m { overflow=True }
          else incIp $ srmRegister RA (fst $ fromInt b a') $ srmRegister RX (fst $ fromInt b x') m

      NCH   -> undefined -- NUM, CHAR, HLT
      SHIFT -> undefined
      MOVE  -> undefined

      JUMP  -> undefined -- JMP, JSJ, JOV, JNOV
      JR r  -> undefined

      IDE r -> undefined -- INC, DEC, ENT, ENN
      CMP r -> undefined

      IN    -> undefined
      OUT   -> undefined
      IOC   -> undefined
      JRED  -> undefined
      JBUS  -> undefined
    where contents = getMemory 
          getAddress :: MixMachine -> Instruction -> Error CheckedAddress
          getAddress m i = checkAddress m $ fromWord a
              where offset = maybe zeroWord (getRegister m) $ iIndex i
                    a = fst $ plus (base m) (iAddr i) offset
          incIp m = Right m { ip=fst $ asInt' (+1) (ip m) }
          asInt' = asInt (base m)
          srmRegister r x m = setRegister m r x
          wsign (MWord Plus  _ _ _ _ _) = 1
          wsign (MWord Minus _ _ _ _ _) = -1
          sign x = if x >= 0 then 1 else -1
