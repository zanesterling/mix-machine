{-# LANGUAGE GADTs #-}
module Lib
    ( someFunc
    ) where

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
                             , ip :: MAddress
                             , memory :: [MWord] -- TODO: Change to random-access array or tree for better memory & runtime performance.
                             , overflow :: Bool
                             , compareFlag :: MComparison
                             , byteMode :: ByteMode
                             }

data MWord = MWord { mws :: MSign
                   , mwb1 :: MByte
                   , mwb2 :: MByte
                   , mwb3 :: MByte
                   , mwb4 :: MByte
                   , mwb5 :: MByte
                   }
data MOffset = MOffset { mos :: MSign
                       , mob1 :: MByte
                       , mob2 :: MByte
                       } deriving (Eq, Show, Read)
data MAddress = MAddress { mas :: MSign
                         , mab1 :: MByte
                         , mab2 :: MByte
                         } deriving (Eq, Show, Read)
data MSign = Plus | Minus deriving (Eq, Show, Read)
type MByte = Int
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
base :: MixMachine -> Int
base m = case byteMode m of
    Tens -> 100
    Twos -> 64

opcodesInOrder = [ NOP, ADD, SUB, MUL, DIV, NCH, SHIFT, MOVE
                 , LD RA, LD RI1, LD RI2, LD RI3, LD RI4, LD RI5, LD RI6, LD RX
                 , LDN RA, LDN RI1, LDN RI2, LDN RI3, LDN RI4, LDN RI5, LDN RI6, LDN RX
                 , ST RA, ST RI1, ST RI2, ST RI3, ST RI4, ST RI5, ST RI6, ST RX , ST RJ
                 , STZ, JBUS, IOC, IN, OUT
                 , JRED, JUMP, JR RA, JR RI1, JR RI2, JR RI3, JR RI4, JR RI5, JR RI6, JR RX
                 , IDE RA, IDE RI1, IDE RI2, IDE RI3, IDE RI4, IDE RI5, IDE RI6, IDE RX
                 , CMP RA, CMP RI1, CMP RI2, CMP RI3, CMP RI4, CMP RI5, CMP RI6, CMP RX]

class Wordy a where
    toWord :: a -> MWord
instance Wordy MOffset where
    toWord (MOffset s b1 b2) = MWord s 0 0 0 b1 b2
instance Wordy MAddress where
    toWord (MAddress s b1 b2) = MWord s 0 0 0 b1 b2

data Instruction = Instruction { iAddr :: MAddress
                               , iIndex :: MByte
                               , iMode :: (Int, Int)
                               , iOpc :: Opcode
                               }
readInstruction :: MWord -> Maybe Instruction
readInstruction (MWord s b1 b2 b3 b4 b5) =
    Just . Instruction (MAddress s b1 b2) b3 (decodeMode b4) =<< decodeOpcode b5
decodeMode :: MByte -> (Int, Int)
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
            NOP   -> undefined
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
    where contents m p = memory m !! toNumber (base m) p
          toNumber b (MAddress s b1 b2) =
              (case s of Plus -> 1; Minus -> -1) * (b1 * b + b2)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
