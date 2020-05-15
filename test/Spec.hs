import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Exception (evaluate)
import Data.Int (Int8, Int64)

import Mix

main :: IO ()
main = hspec $ do
    describe "Mix.toInt" $ do
        it "converts Plus 53 21 to 5321" $
            toInt 100 (MWord Plus 0 0 0 53 21) `shouldBe` (5321 :: Int64)
        it "converts Minus 53 21 to -5321" $
            toInt 100 (MWord Minus 0 0 0 53 21) `shouldBe` (-5321 :: Int64)
        it "converts Plus 16 0 to 1024 under base 64" $
            toInt 64  (MWord Plus 0 0 0 16 0) `shouldBe` (1024 :: Int64)

    describe "Mix.fromInt" $ do
        it "works for decimals" $
            fromInt 100 5321 `shouldBe` (MWord Plus 0 0 0 53 21, 0)
        it "works for binary" $
            fromInt 64 1025 `shouldBe` (MWord Plus 0 0 0 16 1, 0)
        it "gives a valid overflow" $
            fromInt 10 212345 `shouldBe` (MWord Plus 1 2 3 4 5, 2)
        it "also overflows right in the negative" $
            fromInt 10 (-212345) `shouldBe` (MWord Minus 1 2 3 4 5, -2)

    describe "Mix.toInt . Mix.fromInt" $
        it "is the identity function" $
            forAll ((,) <$> choose (2, 128) <*> choose (-128^5+1, 128^5-1)) $
            \(b, x) -> abs x < b^5 ==> toInt b (fst $ fromInt b x) == x
