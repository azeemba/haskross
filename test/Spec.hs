import Test.Hspec
import Control.Exception (evaluate)
import Lib

main :: IO ()
main = hspec $ do
  describe "does_word_fit" $ do
    it "handles simple positive case" $ do
      does_word_fit 'a' 1 "_a_" `shouldBe` True
    it "handles index larger than word size" $ do
      does_word_fit 'a' 3 "_a_" `shouldBe` False
    it "handles simple negative case" $ do
      does_word_fit 'a' 1 "___" `shouldBe` False

  describe "does_char_fit" $ do
    it "handles empty words list" $ do
      does_char_fit 'a' 1 [] `shouldBe` False
    it "returns true when char matches a words" $ do
      does_char_fit 'a' 1 ["_a_"] `shouldBe` True
    it "returns true even with multiple words" $ do
      does_char_fit 'a' 1 ["_a_", "____", "_"] `shouldBe` True
    it "returns false when no words match" $ do
      does_char_fit 'a' 1 ["bbb", "notmatching", "_"] `shouldBe` False

  describe "filter_node" $ do
    it "results in 0 chars when word list emptyh" $ do
      filter_node (Node ['a'] 0 1) [] `shouldBe` Node [] 0 1
    it "should have chars empty if they start empty" $ do
      filter_node (Node [] 0 1) ["abc"] `shouldBe` Node [] 0 1
    it "should remove characters with no match" $ do
      filter_node (Node ['a', 'b', 'd'] 0 1) ["abc", "bac"]
        `shouldBe` Node ['a', 'b'] 0 1
    it "should remove characters with only row or only column match" $ do
      filter_node (Node ['a', 'b', 'd'] 0 1) ["abc"] `shouldBe` Node [] 0 1
