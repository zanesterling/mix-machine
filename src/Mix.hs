{-# LANGUAGE GADTs #-}
module Mix where

import Data.Int (Int8, Int64)
import Debug.Trace

-- IDEA: Purely functional programming means easy to keep old states b/c of pointer machine.
--       Record every state and allow reversing etc?

data MixMachine = MixMachine { rA  :: MWord
                             , rX  :: MWord
                             , rI1 :: MOffset
                             , rI2 :: MOffset
                             , rI3 :: MOffset
                             , rI4 :: MOffset
                             , rI5 :: MOffset
                             , rI6 :: MOffset
                             , rJ  :: MAddress
                             , ip  :: MAddress
                             , memory :: [MWord] -- TODO: Change to random-access array or tree for better memory & runtime performance.
                             , overflow :: Bool
                             , compareFlag :: MComparison
                             , byteMode :: ByteMode
                             }

getRegister :: MixMachine -> Register a -> MWord
getRegister m r =
    case r of
        RA -> rA m
        RX -> rX m
        RJ -> toWord $ rJ m
        RI1 -> toWord $ rI1 m
        RI2 -> toWord $ rI2 m
        RI3 -> toWord $ rI3 m
        RI4 -> toWord $ rI4 m
        RI5 -> toWord $ rI5 m
        RI6 -> toWord $ rI6 m
setRegister :: MixMachine -> Register a -> MWord -> MixMachine
setRegister m r w@(MWord s b1 b2 b3 b4 b5) =
    case r of
        RA  -> m { rA=w }
        RX  -> m { rX=w }
        RJ  -> m { rJ=MAddress s b4 b5 }
        RI1 -> m { rI1=MOffset s b4 b5 }
        RI2 -> m { rI2=MOffset s b4 b5 }
        RI3 -> m { rI3=MOffset s b4 b5 }
        RI4 -> m { rI4=MOffset s b4 b5 }
        RI5 -> m { rI5=MOffset s b4 b5 }
        RI6 -> m { rI6=MOffset s b4 b5 }

data MWord = MWord { mws :: MSign
                   , mwb1 :: MByte
                   , mwb2 :: MByte
                   , mwb3 :: MByte
                   , mwb4 :: MByte
                   , mwb5 :: MByte
                   } deriving (Eq, Show, Read)
plus :: Int64 -> MWord -> MWord -> (MWord, Bool)
plus base w1 w2 = (w', o > 0)
    where x1 = toInt base w1
          x2 = toInt base w2
          (w', o) = fromInt base $ x1 + x2
times :: Int64 -> MWord -> MWord -> (MWord, MWord)
times base w1 w2 = (w'1, w'2)
    where x1 = toInt base w1
          x2 = toInt base w2
          (w'1, o) = fromInt base $ x1 * x2
          (w'2, _) = fromInt base o
negate :: MWord -> MWord
negate (MWord Plus  b1 b2 b3 b4 b5) = MWord Minus b1 b2 b3 b4 b5
negate (MWord Minus b1 b2 b3 b4 b5) = MWord Plus  b1 b2 b3 b4 b5

toInt :: Int64 -> MWord -> Int64
toInt base (MWord s b1 b2 b3 b4 b5) = sign s * ((((b1' * base + b2') * base + b3') * base + b4') * base + b5')
    where sign Plus  = 1
          sign Minus = -1
          b1' = fromIntegral b1
          b2' = fromIntegral b2
          b3' = fromIntegral b3
          b4' = fromIntegral b4
          b5' = fromIntegral b5
fromInt :: Int64 -> Int64 -> (MWord, Int64)
fromInt base x = (MWord s b1 b2 b3 b4 b5, overflow)
    where s      = if x >= 0 then Plus else Minus
          sign x = if x >= 0 then 1    else -1
          a5 = abs x
          a4 = a5 `quot` base
          a3 = a4 `quot` base
          a2 = a3 `quot` base
          a1 = a2 `quot` base
          overflow = sign x * a1 `quot` base
          b5 = fromIntegral $ a5 `rem` base
          b4 = fromIntegral $ a4 `rem` base
          b3 = fromIntegral $ a3 `rem` base
          b2 = fromIntegral $ a2 `rem` base
          b1 = fromIntegral $ a1 `rem` base

data MOffset = MOffset { mos :: MSign
                       , mob1 :: MByte
                       , mob2 :: MByte
                       } deriving (Eq, Show, Read)
data MAddress = MAddress { mas :: MSign
                         , mab1 :: MByte
                         , mab2 :: MByte
                         } deriving (Eq, Show, Read)
data MSign = Plus | Minus deriving (Eq, Show, Read)
type MByte = Int8
data MComparison = Less | Equal | Greater deriving (Eq, Show, Read)

data Opcode where
    NOP   :: Opcode
    ADD   :: Opcode 
    SUB   :: Opcode 
    MUL   :: Opcode 
    DIV   :: Opcode 
    NCH   :: Opcode 
    SHIFT :: Opcode 
    MOVE  :: Opcode 
    LD    :: Register NotJ -> Opcode
    LDN   :: Register NotJ -> Opcode
    ST    :: Register a -> Opcode
    STZ   :: Opcode
    JBUS  :: Opcode
    IOC   :: Opcode
    IN    :: Opcode
    OUT   :: Opcode
    JRED  :: Opcode
    JUMP  :: Opcode
    JR    :: (MaybeJ a) => Register a -> Opcode
    IDE   :: Register NotJ -> Opcode
    CMP   :: Register NotJ -> Opcode

data Register a where
    RA  :: Register NotJ
    RX  :: Register NotJ
    RI1 :: Register NotJ
    RI2 :: Register NotJ
    RI3 :: Register NotJ
    RI4 :: Register NotJ
    RI5 :: Register NotJ
    RI6 :: Register NotJ
    RJ  :: Register DefJ
data NotJ = NotJ deriving (Eq, Show, Read)
data DefJ = DefJ deriving (Eq, Show, Read)
class MaybeJ a where
instance MaybeJ NotJ where
instance MaybeJ DefJ where

data ByteMode = Tens | Twos
base :: MixMachine -> Int8
base m = case byteMode m of
    Tens -> 100
    Twos -> 64

opcodesInOrder = [ NOP, ADD, SUB, MUL, DIV, NCH, SHIFT, MOVE
                 , LD RA, LD RI1, LD RI2, LD RI3, LD RI4, LD RI5, LD RI6, LD RX
                 , LDN RA, LDN RI1, LDN RI2, LDN RI3, LDN RI4, LDN RI5, LDN RI6, LDN RX
                 , ST RA, ST RI1, ST RI2, ST RI3, ST RI4, ST RI5, ST RI6, ST RX, ST RJ
                 , STZ, JBUS, IOC, IN, OUT
                 , JRED, JUMP, JR RA, JR RI1, JR RI2, JR RI3, JR RI4, JR RI5, JR RI6, JR RX
                 , IDE RA, IDE RI1, IDE RI2, IDE RI3, IDE RI4, IDE RI5, IDE RI6, IDE RX
                 , CMP RA, CMP RI1, CMP RI2, CMP RI3, CMP RI4, CMP RI5, CMP RI6, CMP RX]

class Wordy a where
    toWord :: a -> MWord
instance Wordy MWord where
    toWord = id
instance Wordy MOffset where
    toWord (MOffset s b1 b2) = MWord s 0 0 0 b1 b2
instance Wordy MAddress where
    toWord (MAddress s b1 b2) = MWord s 0 0 0 b1 b2

data Instruction = Instruction { iAddr :: MAddress
                               , iIndex :: MByte
                               , iMode :: (Int8, Int8)
                               , iOpc :: Opcode
                               }
readInstruction :: MWord -> Maybe Instruction
readInstruction (MWord s b1 b2 b3 b4 b5) =
    Just . Instruction (MAddress s b1 b2) b3 (decodeMode b4) =<< decodeOpcode b5
decodeMode :: MByte -> (Int8, Int8)
decodeMode b = (b `quot` 8, b `rem` 8)
decodeOpcode :: MByte -> Maybe Opcode
decodeOpcode b = opcodesInOrder `safeIndex` b
    where safeIndex [] _ = Nothing
          safeIndex (x:_) 0 = Just x
          safeIndex (_:xs) b = xs `safeIndex` (b-1)

stepMachine :: MixMachine -> Either String MixMachine
stepMachine m =
    let instr = readInstruction $ contents m $ ip m
    in case instr of
        Nothing -> Left "bad instruction read"
        Just i -> case iOpc i of
            NOP   -> Right $ m { ip=ip m }
            ADD   -> undefined
            SUB   -> undefined
            MUL   -> undefined
            DIV   -> undefined
            NCH   -> undefined
            SHIFT -> undefined
            MOVE  -> undefined
            LD r  -> undefined
            LDN r -> undefined
            ST r  -> undefined
            STZ   -> undefined
            JBUS  -> undefined
            IOC   -> undefined
            IN    -> undefined
            OUT   -> undefined
            JRED  -> undefined
            JUMP  -> undefined
            JR r  -> undefined
            IDE r -> undefined
            CMP r -> undefined
    where contents m p = memory m !! fromIntegral (toNumber (base m) p)
          toNumber b (MAddress s b1 b2) =
              (case s of Plus -> 1; Minus -> -1) * (b1 * b + b2)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
