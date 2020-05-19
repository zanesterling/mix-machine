import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Exception (evaluate)
import Data.Int (Int8, Int64)

import MixInternal
import Mix

main :: IO ()
main = hspec $ 
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
