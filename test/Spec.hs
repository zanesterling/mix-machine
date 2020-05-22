import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Exception (evaluate)
import Data.Int (Int8, Int64)
import Data.Either (fromRight, isRight)

import MixInternal
import Mix

main :: IO ()
main = hspec $ do
    describe "MixInternal" $ do
        describe "getField" $ do
            let w = MWord Minus 1 2 3 5 4
            it "is identity with full field" $
                getField (0, 5) w `shouldBe` w
            it "drops sign with (1,5)" $
                getField (1, 5) w `shouldBe` MWord Plus  1 2 3 5 4
            it "takes only what's specified" $
                getField (3, 5) w `shouldBe` MWord Plus  0 0 3 5 4
            it "shifts selection down and fills" $
                getField (0, 3) w `shouldBe` MWord Minus 0 0 1 2 3
            it "shifts selection down" $
                getField (4, 4) w `shouldBe` MWord Plus  0 0 0 0 5
            it "can take just the sign" $
                getField (0, 0) w `shouldBe` MWord Minus 0 0 0 0 0

        describe "setField" $ do
            let dst = MWord Minus 1 2 3 4 5
            let src = MWord Plus  6 7 8 9 0
            it "sets full with full field" $
                setField (0, 5) dst src `shouldBe` src
            it "skips sign if l /= 0" $
                setField (1, 5) dst src `shouldBe` MWord Minus 6 7 8 9 0
            it "can set one byte at the end" $
                setField (5, 5) dst src `shouldBe` MWord Minus 1 2 3 4 0
            it "can set one byte at the middle" $
                setField (2, 2) dst src `shouldBe` MWord Minus 1 0 3 4 5
            it "can set two bytes at the middle" $
                setField (2, 3) dst src `shouldBe` MWord Minus 1 9 0 4 5
            it "can set sign and first byte" $
                setField (0, 1) dst src `shouldBe` MWord Plus  0 2 3 4 5

        describe "toInt" $ do
            it "converts Plus 53 21 to 5321" $
                toInt 100 (MWord Plus 0 0 0 53 21) `shouldBe` (5321 :: Int64)
            it "converts Minus 53 21 to -5321" $
                toInt 100 (MWord Minus 0 0 0 53 21) `shouldBe` (-5321 :: Int64)
            it "converts Plus 16 0 to 1024 under base 64" $
                toInt 64  (MWord Plus 0 0 0 16 0) `shouldBe` (1024 :: Int64)

        describe "fromInt" $ do
            it "works for decimals" $
                fromInt 100 5321 `shouldBe` (MWord Plus 0 0 0 53 21, 0)
            it "works for binary" $
                fromInt 64 1025 `shouldBe` (MWord Plus 0 0 0 16 1, 0)
            it "gives a valid overflow" $
                fromInt 10 212345 `shouldBe` (MWord Plus 1 2 3 4 5, 2)
            it "also overflows right in the negative" $
                fromInt 10 (-212345) `shouldBe` (MWord Minus 1 2 3 4 5, -2)

        describe "toInt . fromInt" $
            it "is the identity function" $
                forAll ((,) <$> choose (2, 128) <*> choose (-128^5+1, 128^5-1)) $
                \(b, x) -> abs x < b^5 ==> toInt b (fst $ fromInt b x) == x

    describe "Mix" $ do
        let m0 = makeMachine 10 Tens
        let b = base m0
        let (Right ca0) = checkAddress m0 (fromWord $ fst $ fromInt b 0)
        let (Right ca1) = checkAddress m0 (fromWord $ fst $ fromInt b 1)
        let m = setRegister m0 RA $ fst $ fromInt b 1
        let m' = setMemory' m (fst $ fromInt b (-2)) ca1
        describe "LD" $ do
            let ldaInstr = writeInstruction $ Instruction (fromWord $ fst $ fromInt b 1) Nothing (0, 5) (LD RA)
            let mm = stepMachine $ setMemory' m' ldaInstr ca0
            it "succeeds" $
                isRight mm `shouldBe` True
            let (Right mm') = mm
            it "loads into RA" $
                toInt b (getRegister mm' RA) `shouldBe` -2

        describe "ST" $ do
            let staInstr = writeInstruction $ Instruction (fromWord $ fst $ fromInt b 1) Nothing (0, 5) (ST RA)
            let mm = stepMachine $ setMemory' m' staInstr ca0
            it "succeeds" $
                isRight mm `shouldBe` True
            let (Right mm') = mm
            it "stores RA to memory address 1" $
                toInt b (getMemory mm' (0, 5) ca1) `shouldBe` 1

        describe "ADD" $ do
            let addInstr = writeInstruction $ Instruction (fromWord $ fst $ fromInt b 1) Nothing (0, 5) ADD
            let mm = stepMachine $ setMemory' m' addInstr ca0
            it "succeeds" $
                isRight mm `shouldBe` True
            let (Right mm') = mm
            it "adds two things together" $
                toInt b (getRegister mm' RA) `shouldBe` -1

        describe "SUB" $ do
            let subInstr = writeInstruction $ Instruction (fromWord $ fst $ fromInt b 1) Nothing (0, 5) SUB
            let mm = stepMachine $ setMemory' m' subInstr ca0
            it "succeeds" $
                isRight mm `shouldBe` True
            let (Right mm') = mm
            it "subtracts memory from RA" $
                toInt b (getRegister mm' RA) `shouldBe` 3

        describe "MUL" $ do
            let mulInstr = writeInstruction $ Instruction (fromWord $ fst $ fromInt b 1) Nothing (0, 5) MUL
            let mm = stepMachine $ setMemory' m' mulInstr ca0
            it "succeeds" $
                isRight mm `shouldBe` True
            let (Right mm') = mm
            it "multiplies memory with RA and puts it in RA:RX" $
                (toInt b (getRegister mm' RA), toInt b (getRegister mm' RX)) `shouldBe` (0, -2)

        describe "DIV" $ do
            let divInstr = writeInstruction $ Instruction (fromWord $ fst $ fromInt b 1) Nothing (0, 5) DIV
            let mm = stepMachine $ setMemory' m' divInstr ca0
            it "succeeds" $
                isRight mm `shouldBe` True
            let (Right mm') = mm
            it "divides RAX with memory and puts it in RA:RX" $
                (toInt b (getRegister mm' RA), toInt b (getRegister mm' RX)) `shouldBe` (b^5 `quot` (-2), 0)