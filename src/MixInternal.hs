{-# LANGUAGE GADTs #-}
module MixInternal ( Error

                   --- NUMBERS ---
                   , Wordy (toWord, fromWord)
                   , MWord (MWord)
                   , MByte
                   , zeroWord
                   , MOffset
                   , MAddress
                   , MSign (Plus, Minus)
                   , MComparison
                   , toInt, fromInt
                   , asInt
                   , mash
                   , plus
                   , times
                   , wNegate

                   --- MIX MACHINE ---
                   , MixMachine (overflow, ip, compareFlag)
                   , base
                   , makeMachine
                   , getRegister, setRegister
                   , getMemory, setMemory
                   , memsize
                   , CheckedAddress
                   , checkAddress

                   --- INSTRUCTION ---
                   , Instruction (iAddr, iIndex, iMode, iOpc)
                   , readInstruction, writeInstruction
                   , Register (RA, RX, RI1, RI2, RI3, RI4, RI5, RI6, RJ)
                   , Opcode (NOP, ADD, SUB, MUL, DIV, NCH, SHIFT, MOVE, LD, LDN, ST, STZ, JBUS, IOC, IN, OUT, JRED, JUMP, JR, IDE, CMP)
                   , TRAX
                   , TRIndex
                   , TRJ
                   , MaybeJ
                   , NotJ
                   ) where

import Data.Int (Int8, Int64)


type Error a = Either String a

--- NUMBERS ---

data MWord = MWord { mws :: MSign
                   , mwb1 :: MByte
                   , mwb2 :: MByte
                   , mwb3 :: MByte
                   , mwb4 :: MByte
                   , mwb5 :: MByte
                   } deriving (Eq, Show, Read)
zeroWord = MWord Plus 0 0 0 0 0

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

class Wordy a where
    toWord :: a -> MWord
    fromWord :: MWord -> a
    add :: Int64 -> Int64 -> a -> a
    add b n w = fromWord $ fst $ fromInt b $ n + toInt b (toWord w)
instance Wordy MWord where
    toWord = id
    fromWord = id
instance Wordy MOffset where
    toWord   (MOffset s     b1 b2) = MWord s 0 0 0 b1 b2
    fromWord (MWord s _ _ _ b4 b5) = MOffset s b4 b5
instance Wordy MAddress where
    toWord   (MAddress s    b1 b2) = MWord s 0 0 0 b1 b2
    fromWord (MWord s _ _ _ b4 b5) = MAddress s b4 b5
instance Wordy CheckedAddress where
    toWord (CheckedAddress a) = toWord a
    fromWord w = CheckedAddress $ fromWord w

asInt :: (Wordy a) => Int64 -> (Int64 -> Int64) -> a -> (a, Int64)
asInt b f w = mapFst fromWord $ fromInt b $ f $ toInt b $ toWord w
    where mapFst f (a, b) = (f a, b)
mash :: (Wordy a, Wordy b) => (Int64 -> Int64 -> c) -> Int64 -> a -> b -> c
mash f b w1 w2 = f (toInt b $ toWord w1) (toInt b $ toWord w2)
plus :: (Wordy a, Wordy b) => Int64 -> a -> b -> (MWord, Bool)
plus b = mash (\x y -> (>0) <$> fromInt b (x + y)) b
times :: (Wordy a, Wordy b) => Int64 -> a -> b -> (MWord, MWord)
times b = mash (\x y -> fst . fromInt b <$> fromInt b (x * y)) b
wNegate (MWord Plus  b1 b2 b3 b4 b5) = MWord Minus b1 b2 b3 b4 b5
wNegate (MWord Minus b1 b2 b3 b4 b5) = MWord Plus  b1 b2 b3 b4 b5

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


--- MIX MACHINE ---

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
makeMachine memsize bm = MixMachine { rA =zeroWord
                                    , rX =zeroWord
                                    , rI1=fromWord zeroWord
                                    , rI2=fromWord zeroWord
                                    , rI3=fromWord zeroWord
                                    , rI4=fromWord zeroWord
                                    , rI5=fromWord zeroWord
                                    , rI6=fromWord zeroWord
                                    , rJ =fromWord zeroWord
                                    , ip =fromWord zeroWord
                                    , memory=replicate memsize zeroWord -- TODO: replace with memsize = base^3
                                    , overflow=False
                                    , compareFlag=Equal
                                    , byteMode=bm
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
memsize :: MixMachine -> Int64
memsize = fromIntegral . length . memory

getMemory :: MixMachine -> CheckedAddress -> MWord
getMemory m a = memory m !! addr
    where addr = fromIntegral (toInt (fromIntegral $ base m) (toWord a))
setMemory :: MixMachine -> MWord -> CheckedAddress -> MixMachine
setMemory m x a = m { memory = setAt (memory m) addr x }
    where addr = fromIntegral (toInt (fromIntegral $ base m) (toWord a))
          setAt l i x = take i l ++ [x] ++ drop (i+1) l

checkAddress :: MixMachine -> MAddress -> Error CheckedAddress
checkAddress m a = if 0 <= a' && a' < memorySize
                   then Right $ CheckedAddress a
                   else Left $ "address " ++ show a ++ " out of bounds"
    where a' = toInt (fromIntegral $ base m) $ toWord a
          memorySize = fromIntegral $ length $ memory m
newtype CheckedAddress = CheckedAddress MAddress


data ByteMode = Tens | Twos
base :: MixMachine -> Int64
base m = case byteMode m of
    Tens -> 100
    Twos -> 64


--- INSTRUCTION ---

data Instruction = Instruction { iAddr :: MAddress
                               , iIndex :: Maybe (Register TRIndex)
                               , iMode :: (MByte, MByte)
                               , iOpc :: Opcode
                               }

readInstruction :: MWord -> Error Instruction
readInstruction (MWord s b1 b2 b3 b4 b5) =
    Right . Instruction (MAddress s b1 b2) (decodeIndex b3) (decodeMode b4) =<< decodeOpcode b5
    where
        decodeMode b = (b `quot` 8, b `rem` 8)
        decodeOpcode b = opcodesInOrder `safeIndex` b
            where safeIndex [] _ = Left $ "bad opcode: " ++ show b
                  safeIndex (x:_) 0 = Right x
                  safeIndex (_:xs) b = xs `safeIndex` (b-1)
        decodeIndex x | 0 < x && x < 7 = Just $ [RI1, RI2, RI3, RI4, RI5, RI6] !! (fromIntegral x - 1)
        decodeIndex _                  = Nothing
writeInstruction :: Instruction -> MWord
writeInstruction (Instruction (MAddress s b1 b2) index (b41, b42) opcode) = MWord s b1 b2 b3 b4 b5
    where b3 = maybe 0 indexRegToInt index
          indexRegToInt ri = opcodeToInt (LD ri) - opcodeToInt (LD RI1) + 1
          b4 = b41 * 8 + b42
          b5 = opcodeToInt opcode
          find (x:xs) y = if x == y then 0 else 1 + find xs y

opcodesInOrder = [ NOP, ADD, SUB, MUL, DIV, NCH, SHIFT, MOVE
                 , LD RA, LD RI1, LD RI2, LD RI3, LD RI4, LD RI5, LD RI6, LD RX
                 , LDN RA, LDN RI1, LDN RI2, LDN RI3, LDN RI4, LDN RI5, LDN RI6, LDN RX
                 , ST RA, ST RI1, ST RI2, ST RI3, ST RI4, ST RI5, ST RI6, ST RX, ST RJ
                 , STZ, JBUS, IOC, IN, OUT
                 , JRED, JUMP, JR RA, JR RI1, JR RI2, JR RI3, JR RI4, JR RI5, JR RI6, JR RX
                 , IDE RA, IDE RI1, IDE RI2, IDE RI3, IDE RI4, IDE RI5, IDE RI6, IDE RX
                 , CMP RA, CMP RI1, CMP RI2, CMP RI3, CMP RI4, CMP RI5, CMP RI6, CMP RX]
opcodeToInt NOP = 0
opcodeToInt ADD = 1
opcodeToInt SUB = 2
opcodeToInt MUL = 3
opcodeToInt DIV = 4
opcodeToInt NCH = 5
opcodeToInt SHIFT = 6
opcodeToInt MOVE = 7
opcodeToInt (LD RA) = 8
opcodeToInt (LD RI1) = 9
opcodeToInt (LD RI2) = 10
opcodeToInt (LD RI3) = 11
opcodeToInt (LD RI4) = 12
opcodeToInt (LD RI5) = 13
opcodeToInt (LD RI6) = 14
opcodeToInt (LD RX) = 15
opcodeToInt (LDN RA) = 16
opcodeToInt (LDN RI1) = 17
opcodeToInt (LDN RI2) = 18
opcodeToInt (LDN RI3) = 19
opcodeToInt (LDN RI4) = 20
opcodeToInt (LDN RI5) = 21
opcodeToInt (LDN RI6) = 22
opcodeToInt (LDN RX) = 23
opcodeToInt (ST RA) = 24
opcodeToInt (ST RI1) = 25
opcodeToInt (ST RI2) = 26
opcodeToInt (ST RI3) = 27
opcodeToInt (ST RI4) = 28
opcodeToInt (ST RI5) = 29
opcodeToInt (ST RI6) = 30
opcodeToInt (ST RX) = 31
opcodeToInt (ST RJ) = 32
opcodeToInt STZ = 33
opcodeToInt JBUS = 34
opcodeToInt IOC = 35
opcodeToInt IN = 36
opcodeToInt OUT = 37
opcodeToInt JRED = 38
opcodeToInt JUMP = 39
opcodeToInt (JR RA) = 40
opcodeToInt (JR RI1) = 41
opcodeToInt (JR RI2) = 42
opcodeToInt (JR RI3) = 43
opcodeToInt (JR RI4) = 44
opcodeToInt (JR RI5) = 45
opcodeToInt (JR RI6) = 46
opcodeToInt (JR RX) = 47
opcodeToInt (IDE RA) = 48
opcodeToInt (IDE RI1) = 49
opcodeToInt (IDE RI2) = 50
opcodeToInt (IDE RI3) = 51
opcodeToInt (IDE RI4) = 52
opcodeToInt (IDE RI5) = 53
opcodeToInt (IDE RI6) = 54
opcodeToInt (IDE RX)  = 55
opcodeToInt (CMP RA)  = 56
opcodeToInt (CMP RI1) = 57
opcodeToInt (CMP RI2) = 58
opcodeToInt (CMP RI3) = 59
opcodeToInt (CMP RI4) = 60
opcodeToInt (CMP RI5) = 61
opcodeToInt (CMP RI6) = 62
opcodeToInt (CMP RX)  = 63

data Opcode where
    NOP   :: Opcode
    ADD   :: Opcode
    SUB   :: Opcode
    MUL   :: Opcode
    DIV   :: Opcode
    NCH   :: Opcode
    SHIFT :: Opcode
    MOVE  :: Opcode
    LD    :: NotJ a => Register a -> Opcode
    LDN   :: NotJ a => Register a -> Opcode
    ST    :: Register a -> Opcode
    STZ   :: Opcode
    JBUS  :: Opcode
    IOC   :: Opcode
    IN    :: Opcode
    OUT   :: Opcode
    JRED  :: Opcode
    JUMP  :: Opcode
    JR    :: (MaybeJ a) => Register a -> Opcode
    IDE   :: (NotJ a)   => Register a -> Opcode
    CMP   :: (NotJ a)   => Register a -> Opcode

data Register a where
    RA  :: Register TRAX
    RX  :: Register TRAX
    RI1 :: Register TRIndex
    RI2 :: Register TRIndex
    RI3 :: Register TRIndex
    RI4 :: Register TRIndex
    RI5 :: Register TRIndex
    RI6 :: Register TRIndex
    RJ  :: Register TRJ
data TRIndex = TRIndex deriving (Eq)
data TRAX = TRAX deriving (Eq)
data TRJ = TRJ deriving (Eq)
class NotJ a where
instance NotJ TRIndex where
instance NotJ TRAX where
class MaybeJ a where
instance MaybeJ TRIndex where
instance MaybeJ TRAX where
instance MaybeJ TRJ where
