{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Ini
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

main :: IO ()
main = hspec $ do
  describe "ini parser" $ do
    it "parse header" $ do
      parse parseHeader "" "[profile yay]"
        `shouldParse` Header "profile yay"

    it "parse assignment" $ do
      parse parseAssignment "" "a = b ; comment\n" `shouldParse` ("a", "b")

    it "parse assignments" $ do
      parse parseAssignments "" "a = b ; comment\nc = d\n"
        `shouldParse` [("a", "b"), ("c", "d")]

    it "parse assignments empty" $ do
      parse parseAssignments "" "; comment\n"
        `shouldParse` []

    it "parse section" $ do
      parse parseSection "" ";comment\n[section] ;comment\na = b\n"
        `shouldParse` (Header "section", [("a", "b")])

    it "parse sections" $ do
      parse (many parseSection) "" "[section1]\n a=b\n c=d\n[section2] ;comment\n"
        `shouldParse` [ (Header "section1", [("a", "b"), ("c", "d")])
                      , (Header "section2", [])
                      ]

    it "parse ini default + multiple sections" $ do
      parse parseIni' "" "a = b ;comment\n[section] \nc = d\ne=f\n[other section]h=i"
        `shouldParse` [ (Header "default", [("a", "b")])
                      , (Header "section", [("c", "d"), ("e", "f")])
                      , (Header "other section", [("h", "i")])
                      ]

    it "parse ini default only" $ do
      parse parseIni' "" "a = b ;comment\n"
        `shouldParse` [ (Header "default", [("a", "b")])
                      ]
