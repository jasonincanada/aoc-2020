import Control.Arrow   ((>>>), (&&&))
import Test.Hspec
import NanoParsec

import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08 hiding (path)
import Day09
import Day10
import Day11


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


  describe "Day03" $ do

    it "Part 1" $ do
      file <- readFile (path ++ "3.txt")
      (Day03.parse >>> Day03.calc1 >>> show) file `shouldBe` "7"

    it "Part 2" $ do
      file <- readFile (path ++ "3.txt")
      (Day03.parse >>> Day03.calc2 >>> show) file `shouldBe` "336"


  describe "Day04" $ do

    it "Part 1" $ do
      file <- readFile (path ++ "4.txt")
      (Day04.parse1 >>> Day04.calc1 >>> show) file `shouldBe` "2"

    it "Part 2" $ do
      file <- readFile (path ++ "4.txt")
      (Day04.parse2 >>> Day04.calc2 >>> show) file `shouldBe` "2"

    it "birthYear valid"   $ try birthYear "byr:2002" `shouldBe` (Just $ Byr 2002)
    it "birthYear invalid" $ try birthYear "byr:2003" `shouldBe` Nothing
    it "birthYear invalid" $ try birthYear "byr:1919" `shouldBe` Nothing

    it "height cm valid"   $ try height "hgt:150cm" `shouldBe` (Just $ Hgt 150 CM)
    it "height cm invalid" $ try height "hgt:194cm" `shouldBe` Nothing
    it "height in valid"   $ try height "hgt:59in"  `shouldBe` (Just $ Hgt 59 IN)
    it "height in invalid" $ try height "hgt:77in"  `shouldBe` Nothing

    it "hairColor valid"   $ try hairColor "hcl:#123abc" `shouldBe` (Just $ Hcl "123abc")
    it "hairColor invalid" $ try hairColor "hcl:#123abz" `shouldBe` Nothing
    it "hairColor invalid" $ try hairColor "hcl:123abc"  `shouldBe` Nothing

    it "eyeColor valid"    $ try eyeColor "ecl:amb" `shouldBe` (Just $ Ecl "amb")
    it "eyeColor invalid"  $ try eyeColor "ecl:wat" `shouldBe` Nothing

    it "passport valid"    $ try passport "pid:000000001"  `shouldBe` (Just $ Pid "000000001")
    it "passport invalid"  $ try passport "pid:0123456789" `shouldBe` Nothing

    it "country valid"     $ try country "cid:11" `shouldBe` (Just $ Cid 11)


  describe "Day05" $ do

    it "decode 1" ( Day05.decode "FBFBBFFRLR" `shouldBe` 357)
    it "decode 2" ( Day05.decode "BFFFBBFRRR" `shouldBe` 567)
    it "decode 3" ( Day05.decode "FFFBBBFRRR" `shouldBe` 119)
    it "decode 4" ( Day05.decode "BBFFBBFRLL" `shouldBe` 820)


  describe "Day06" $ do

    it "Part 1" $ do
      file <- readFile (path ++ "6.txt")
      (Day06.parse >>> Day06.calc1 >>> show) file `shouldBe` "11"

    it "Part 2" $ do
      file <- readFile (path ++ "6.txt")
      (Day06.parse >>> Day06.calc2 >>> show) file `shouldBe` "6"


  describe "Day07" $ do

    it "Part 1" $ do
      file <- readFile (path ++ "7.txt")
      (Day07.parse >>> Day07.calc1 >>> show) file `shouldBe` "4"

    it "Part 2-1" $ do
      file <- readFile (path ++ "7.txt")
      (Day07.parse >>> Day07.calc2 >>> show) file `shouldBe` "32"

    it "Part 2-2" $ do
      file <- readFile (path ++ "7-2.txt")
      (Day07.parse >>> Day07.calc2 >>> show) file `shouldBe` "126"


  describe "Day08" $ do

    it "Part 1" $ do
      file <- readFile (path ++ "8.txt")
      (Day08.parse >>> Day08.calc1 >>> show) file `shouldBe` "5"

    it "Part 2" $ do
      file <- readFile (path ++ "8.txt")
      (Day08.parse >>> Day08.calc2 >>> show) file `shouldBe` "8"


  describe "Day09" $ do

    it "Part 1" $ do
      file <- readFile (path ++ "9.txt")
      (Day09.parse >>> Day09.calc1 >>> show) file `shouldBe` "127"

    it "Part 2" $ do
      file <- readFile (path ++ "9.txt")
      (Day09.parse >>> Day09.calc2 >>> show) file `shouldBe` "62"


  describe "Day10" $ do

    it "Part 1" $ do
      file <- readFile (path ++ "10.txt")
      (Day10.parse >>> Day10.calc1 >>> show) file `shouldBe` "220"

{-
    it "Part 2" $ do
      file <- readFile (path ++ "10.txt")
      (Day10.parse >>> Day10.calc2 >>> show) file `shouldBe` "0"
-}



  describe "Day11" $ do

    it "Part 1" $ do
      file <- readFile (path ++ "11.txt")
      (Day11.parse >>> Day11.calc1 >>> show) file `shouldBe` "37"

    it "Part 2" $ do
      file <- readFile (path ++ "11.txt")
      (Day11.parse >>> Day11.calc2 >>> show) file `shouldBe` "26"

