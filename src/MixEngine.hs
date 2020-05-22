{-# LANGUAGE ScopedTypeVariables #-}
module MixEngine where

import MixInternal

-- IDEA: Purely functional programming means easy to keep old states b/c of pointer machine.
--       Record every state and allow reversing etc?

stepMachine :: MixMachine -> Error MixMachine
stepMachine m = do
    instr <- readInstruction =<< (getMemory' m <$> checkAddress m (ip m))
    let f = iMode instr
        b = base m
        jumpIf p m = if p m
                     then return $ srmRegister RJ (toWord $ ip m) $ m { ip=iAddr instr }
                     else incIp m
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

      NCH   ->
        case f of
          (0, 0) -> undefined -- NUM
          (0, 1) -> undefined -- CHAR
          (0, 2) -> Left "halted" -- HLT
          _      -> Left ("bad field for NUM/CHAR/HLT instruction: " ++ show f)
      SHIFT -> undefined
      MOVE  -> undefined

      JUMP  ->
        case f of
          -- JMP
          (0, 0) -> jumpIf (const True) m
          -- JSJ
          (0, 1) -> return $ m { ip=iAddr instr }
          -- JOV
          (0, 2) -> jumpIf overflow m
          -- JNOV
          (0, 3) -> jumpIf (not . overflow) m

          -- JL
          (0, 4) -> jumpIf ((==Less)    . compareFlag) m
          -- JE
          (0, 5) -> jumpIf ((==Equal)   . compareFlag) m
          -- JG
          (0, 6) -> jumpIf ((==Greater) . compareFlag) m
          -- JGE
          (0, 7) -> jumpIf ((/=Less)    . compareFlag) m
          -- JNE
          (1, 0) -> jumpIf ((/=Equal)   . compareFlag) m
          -- JLE
          (1, 1) -> jumpIf ((/=Greater) . compareFlag) m
          _      -> Left ("bad field for JUMP instruction: " ++ show f)

      JR r  -> let r' = toInt b $ getRegister m r in
        case f of
          -- JrN
          (0, 0) -> jumpIf (const $ r' < 0) m
          -- JrZ
          (0, 1) -> jumpIf (const $ r' == 0) m
          -- JrP
          (0, 2) -> jumpIf (const $ r' > 0) m
          -- JrN
          (0, 3) -> jumpIf (const $ r' >= 0) m
          -- JrZ
          (0, 4) -> jumpIf (const $ r' /= 0) m
          -- JrP
          (0, 5) -> jumpIf (const $ r' <= 0) m
          _      -> Left ("bad field for JR instruction: " ++ show f)

      IDE r -> case f of
        -- INC
        (0, 0) -> let (a', o) = plus b (getRegister m RA) (iAddr instr)
                  in incIp $ srmRegister RA a' $ m {overflow=o}
        -- DEC
        (0, 1) -> let (a', o) = plus b (getRegister m RA) $ wNegate $ toWord $ iAddr instr
                  in incIp $ srmRegister RA a' $ m {overflow=o}
        -- ENT
        (0, 2) -> incIp $ setRegister m RA $ toWord $ iAddr instr
        -- ENN
        (0, 3) -> incIp $ setRegister m RA $ wNegate $ toWord $ iAddr instr
        _      -> Left ("bad field for INC/DEC/ENT/ENN instruction: " ++ show f)

      CMP r -> do
        v <- getMemory m f <$> getAddress m instr
        let rv=getField f $ getRegister m r
        return m {compareFlag=wCompare b rv v}

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
