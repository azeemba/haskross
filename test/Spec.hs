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

  describe "compress_node" $ do
    it "results in 0 chars when word list emptyh" $ do
      compress_node (Node 0 [] 1 [] ['c']) `shouldBe` Node 0 [] 1 [] []
    it "should have chars empty if they start empty" $ do
      compress_node (Node 1 ["abc"] 0 ["bac"] []) `shouldBe` Node 1 [] 0 [] []
  
  describe "fix_compress" $ do
    it "should leave stable state unchanged" $ do
      fix_compress (Node 0 ["ab", "bc"] 1 ["_a", "_b"] ['a', 'b'])
        `shouldBe` Node 0 ["ab", "bc"] 1 ["_a", "_b"] ['a', 'b']
    it "should remove first clue words with no match" $ do
      fix_compress (Node 0 ["ac", "bd"] 1 ["eb", "fb"] ['a', 'b'])
        `shouldBe` Node 0 ["bd"] 1 ["eb", "fb"] ['b']
    it "should remove second clue words with no match" $ do
      fix_compress (Node 1 ["eb", "fb"] 0 ["ac", "bd"] ['a', 'b'])
        `shouldBe` Node 1 ["eb", "fb"] 0 ["bd"] ['b']
    it "should remove characters with no match" $ do
      compress_node (Node 0 ["a", "b"] 0 ["a", "b"] ['a', 'b', 'd'])
        `shouldBe` Node 0 ["a", "b"] 0 ["a", "b"] ['a', 'b']
