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
data Opcode = NOP | ADD | SUB | MUL | DIV | NUMCHARHALT | SHIFT | MOVE | LDA
            | LD1 | LD2 | LD3 | LD4 | LD5 | LD6 | LDX
            | LDAN | LD1N | LD2N | LD3N | LD4N | LD5N | LD6N | LDXN
            | STA | ST1 | ST2 | ST3 | ST4 | ST5 | ST6 | STX
            | STJ | STZ | JBUS | IOC | IN | OUT
            | JRED | JUMP | JA | J1 | J2 | J3 | J4 | J5 | J6 | JX
            | IDEA | IDE1 | IDE2 | IDE3 | IDE4 | IDE5 | IDE6 | IDEX
            | CMPA | CMP1 | CMP2 | CMP3 | CMP4 | CMP5 | CMP6 | CMPX
data ByteMode = Tens | Twos
base :: MixMachine -> Int
base m = case byteMode m of
    Tens -> 100
    Twos -> 64

opcodesInOrder = [ NOP, ADD, SUB, MUL, DIV, NUMCHARHALT, SHIFT, MOVE, LDA
                 , LD1, LD2, LD3, LD4, LD5, LD6, LDX
                 , LDAN, LD1N, LD2N, LD3N, LD4N, LD5N, LD6N, LDXN
                 , STA, ST1, ST2, ST3, ST4, ST5, ST6, STX
                 , STJ, STZ, JBUS, IOC, IN, OUT
                 , JRED, JUMP, JA, J1, J2, J3, J4, J5, J6, JX
                 , IDEA, IDE1, IDE2, IDE3, IDE4, IDE5, IDE6, IDEX
                 , CMPA, CMP1, CMP2, CMP3, CMP4, CMP5, CMP6, CMPX]

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
stepMachine m = let instr = readInstruction $ contents m $ ip m
                in case iOpc instr of
                    NOP -> undefined
                    ADD -> undefined
                    SUB -> undefined
                    MUL -> undefined
                    DIV -> undefined
                    NUMCHARHALT -> undefined
                    SHIFT -> undefined
                    MOVE -> undefined
                    LDA -> undefined
                    LD1 -> undefined
                    LD2 -> undefined
                    LD3 -> undefined
                    LD4 -> undefined
                    LD5 -> undefined
                    LD6 -> undefined
                    LDX -> undefined
                    LDAN -> undefined
                    LD1N -> undefined
                    LD2N -> undefined
                    LD3N -> undefined
                    LD4N -> undefined
                    LD5N -> undefined
                    LD6N -> undefined
                    LDXN -> undefined
                    STA -> undefined
                    ST1 -> undefined
                    ST2 -> undefined
                    ST3 -> undefined
                    ST4 -> undefined
                    ST5 -> undefined
                    ST6 -> undefined
                    STX -> undefined
                    STJ -> undefined
                    STZ -> undefined
                    JBUS -> undefined
                    IOC -> undefined
                    IN -> undefined
                    OUT -> undefined
                    JRED -> undefined
                    JUMP -> undefined
                    JA -> undefined
                    J1 -> undefined
                    J2 -> undefined
                    J3 -> undefined
                    J4 -> undefined
                    J5 -> undefined
                    J6 -> undefined
                    JX -> undefined
                    IDEA -> undefined
                    IDE1 -> undefined
                    IDE2 -> undefined
                    IDE3 -> undefined
                    IDE4 -> undefined
                    IDE5 -> undefined
                    IDE6 -> undefined
                    IDEX -> undefined
                    CMPA -> undefined
                    CMP1 -> undefined
                    CMP2 -> undefined
                    CMP3 -> undefined
                    CMP4 -> undefined
                    CMP5 -> undefined
                    CMP6 -> undefined
                    CMPX -> undefined


    where contents m p = memory m !! toNumber (base m) p
          toNumber b (MAddress s b1 b2) =
              (case s of Plus -> 1; Minus -> -1) * (b1 * b + b2)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
