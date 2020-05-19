{-# LANGUAGE ScopedTypeVariables #-}
module Mix where

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

getField :: (MByte, MByte) -> MWord -> MWord
getField (l, r) w@(MWord s _ _ _ _ _) =
  listToWord s' $ extend $ sub1 (max l 1) r $ wordToList w
  where s' = if l == 0 then s else Plus
        sub1 l r = sub0 (fromIntegral l-1) (fromIntegral r-1)
        sub0 l r xs = take (r-l+1) $ drop l xs

setField :: (MByte, MByte) -> MWord -> MWord -> MWord
setField (l, r) dst@(MWord sDst _ _ _ _ _) src@(MWord sSrc _ _ _ _ _) =
  listToWord s' $ extend $ setAt (max l' 1 - 1) (wordToList dst) $ takeLast n $ wordToList src
  where s' = if l == 0 then sSrc else sDst
        n = r' - max 1 l' + 1
        (l', r') = (fromIntegral l, fromIntegral r)
        setAt i dst src = take i dst ++ src ++ drop (i + length src) dst
  
wordToList :: MWord -> [MByte]
wordToList (MWord _ b1 b2 b3 b4 b5) = [b1, b2, b3, b4, b5]

newtype CheckedList = CheckedList [MByte]
listToWord :: MSign -> CheckedList -> MWord
listToWord s (CheckedList [b1, b2, b3, b4, b5]) = MWord s b1 b2 b3 b4 b5

extend :: [MByte] -> CheckedList
extend l = CheckedList $ takeLast 5 ([0, 0, 0, 0, 0] ++ l)

takeLast :: Int -> [a] -> [a]
takeLast n l = drop (length l - n) l