import Control.Arrow   ((>>>), (&&&))
import Test.Hspec

import Day02


path = "test/inputs/"

main :: IO ()
main = hspec $ do

  describe "Day02" $ do

    it "Part 1" $ do
      file <- readFile (path ++ "2.txt")
      (Day02.parse >>> Day02.calc1 >>> show) file `shouldBe` "2"

    it "Part 2" $ do
      file <- readFile (path ++ "2.txt")
      (Day02.parse >>> Day02.calc2 >>> show) file `shouldBe` "1"

