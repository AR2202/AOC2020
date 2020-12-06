import Test.Hspec
import Test.QuickCheck

import Common
import Day1 as D1
import Day2 as D2
import Day3 as D3
import Day4 as D4
import Day5 as D5
import Day6 as D6

main :: IO ()
main = hspec $ 
  describe "D4.howManyWithAllValidFields" $ do
      it "returns 0 in invalid inputs" $ do
        invalid <- splitOnBlankLine  "example4binvalid.txt"
        D4.howManyWithAllValidFields invalid `shouldBe` 0
        

      context "when applied to 4 valid inputs" $ do
          it "returns 0 on invalid inputs" $ do
            valid <- splitOnBlankLine  "example4bvalid.txt"
            D4.howManyWithAllValidFields valid `shouldBe` 4



    
prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs