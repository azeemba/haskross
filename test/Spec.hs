import Test.Hspec
import qualified Data.Vector as Vector
import qualified Data.IntSet as IntSet
import Data.Maybe
import Control.Exception (evaluate)
import Lib
import Data.List

make_node :: Int -> [String] -> Int -> [String] -> [Char] -> Node
make_node ind1 words1 ind2 words2 chars =
  let all_words = Vector.fromList (words1 ++ words2)
      wordsI1   = IntSet.fromDistinctAscList (take (length words1) [0 ..])
      wordsI2 =
        IntSet.fromDistinctAscList (take (length words2) [(length words1) ..])
  in  Node ind1 wordsI1 ind2 wordsI2 chars all_words

make_node_with_words
  :: Int
  -> [String]
  -> Int
  -> [String]
  -> [Char]
  -> Vector.Vector String
  -> Node
make_node_with_words ind1 words1 ind2 words2 chars all_words =
  let wordsI1 = IntSet.fromList
        $ map (\word -> fromJust (Vector.elemIndex word all_words)) words1
      wordsI2 = IntSet.fromList
        $ map (\word -> fromJust (Vector.elemIndex word all_words)) words2
  in  Node ind1 wordsI1 ind2 wordsI2 chars all_words


main :: IO ()
main = hspec $ do
  describe "does_word_fit" $ do
    it "handles simple positive case" $ do
      let words = Vector.fromList ["_a_"]
      does_word_fit 'a' 1 words 0 `shouldBe` True
    it "handles index larger than word size" $ do
      let words = Vector.fromList ["_a_"]
      does_word_fit 'a' 3 words 0 `shouldBe` False
    it "handles simple negative case" $ do
      let words = Vector.fromList ["___"]
      does_word_fit 'a' 1 words 0 `shouldBe` False

  describe "does_char_fit" $ do
    it "handles empty words list" $ do
      let words = Vector.fromList []
      does_char_fit 'a' 1 (IntSet.fromDistinctAscList []) words `shouldBe` False
    it "returns true when char matches a words" $ do
      let words  = Vector.fromList ["_a_"]
      let wordsI = IntSet.fromDistinctAscList [0]
      does_char_fit 'a' 1 wordsI words `shouldBe` True
    it "returns true even with multiple words" $ do
      let words  = Vector.fromList ["_a_", "____", "_"]
      let wordsI = IntSet.fromDistinctAscList [0, 1, 2]
      does_char_fit 'a' 1 wordsI words `shouldBe` True
    it "returns false when no words match" $ do
      let words  = Vector.fromList ["bbb", "notmatching", "_"]
      let wordsI = IntSet.fromDistinctAscList [0, 1, 2]
      does_char_fit 'a' 1 wordsI words `shouldBe` False

  describe "compress_node" $ do
    it "results in 0 chars when word list emptyh" $ do
      compress_node (make_node 0 [] 1 [] ['c'])
        `shouldBe` make_node 0 [] 1 [] []
    it "should have chars empty if they start empty" $ do
      compress_node (make_node 1 ["abc"] 0 ["bac"] []) `shouldBe` Node
        1
        IntSet.empty
        0
        IntSet.empty
        []
        (Vector.fromList ["abc", "bac"])

  describe "fix_compress" $ do
    it "should leave stable state unchanged" $ do
      fix_compress (make_node 0 ["ab", "bc"] 1 ["_a", "_b"] ['a', 'b'])
        `shouldBe` make_node 0 ["ab", "bc"] 1 ["_a", "_b"] ['a', 'b']
    it "should remove first clue words with no match" $ do
      let all_words = Vector.fromList ["ac", "bd", "eb", "fb"]
      let wordsI1   = IntSet.fromDistinctAscList [0, 1]
      let wordsI2   = IntSet.fromDistinctAscList [2, 3]
      fix_compress (Node 0 wordsI1 1 wordsI2 ['a', 'b'] all_words)
        `shouldBe` Node 0
                        (IntSet.fromAscList [1])
                        1
                        (IntSet.fromAscList [2, 3])
                        ['b']
                        all_words
    it "should remove second clue words with no match" $ do
      fix_compress (make_node 1 ["eb", "fb"] 0 ["ac", "bd"] ['a', 'b'])
        `shouldBe` make_node_with_words
                     1
                     ["eb", "fb"]
                     0
                     ["bd"]
                     ['b']
                     (Vector.fromList ["eb", "fb", "ac", "bd"])
    it "should remove characters with no match" $ do
      compress_node (make_node 0 ["a", "b"] 0 ["a", "b"] ['a', 'b', 'd'])
        `shouldBe` make_node 0 ["a", "b"] 0 ["a", "b"] ['a', 'b']

  describe "merge nodes in clues" $ do
    it "should remove words that aren't in both nodes" $ do
      let all_words = Vector.fromList ["both", "single", "one", "both"]
      let node1 =
            make_node_with_words 0 ["both", "single"] 0 [] ['a', 'b'] all_words
      let node2 =
            make_node_with_words 0 ["one", "both"] 0 [] ['a', 'b'] all_words
      let result1 = make_node_with_words 0 ["both"] 0 [] ['a', 'b'] all_words
      let result2 = make_node_with_words 0 ["both"] 0 [] ['a', 'b'] all_words
      merge_clues First [node1, node2] `shouldBe` [result1, result2]
    it "should leave valid clues unchanged" $ do
      let words = ["a", "b", "c"]
      let chars = ['a', 'b']
      let node  = make_node 0 words 0 words chars
      merge_clues First [node, node] `shouldBe` [node, node]
    it "should empty out words when any words is empty" $ do
      let words1    = ["ab", "cd", "ef"]
      let words2    = []
      let chars     = ['a' .. 'z']
      let all_words = Vector.fromList words1
      let node1     = make_node_with_words 0 words1 0 words2 chars all_words
      let node2     = make_node_with_words 1 words2 0 words2 chars all_words
      merge_clues First [node1, node2]
        `shouldBe` [ make_node_with_words 0 [] 0 [] chars all_words
                   , make_node_with_words 1 [] 0 [] chars all_words
                   ]
    it "should empty out when no match possible" $ do
      let words1    = ["ab", "cd", "ef"]
      let words2    = ["pq", "rs", "tv"]
      let chars     = ['a' .. 'z']
      let all_words = Vector.fromList (words1 ++ words2)
      let node1     = make_node_with_words 0 words1 0 [] chars all_words
      let node2     = make_node_with_words 1 words2 0 [] chars all_words
      merge_clues First [node1, node2]
        `shouldBe` [ make_node_with_words 0 [] 0 [] chars all_words
                   , make_node_with_words 1 [] 0 [] chars all_words
                   ]

  describe "square grid" $ do
    it "should look right" $ do
      let words  = ["bat", "cat"]
      let wordsV = Vector.fromList words
      let chars  = ['a' .. 'z']
      let nodes = Vector.fromList
            [ make_node_with_words 0 words 0 words chars wordsV
            , make_node_with_words 0 words 1 words chars wordsV
            , make_node_with_words 1 words 0 words chars wordsV
            , make_node_with_words 1 words 1 words chars wordsV
            ]
      let
        clues =
          [(First, [0, 1]), (First, [2, 3]), (Second, [0, 2]), (Second, [1, 3])]
      make_square_grid 2 wordsV `shouldBe` Grid nodes clues

    it "should stabilize" $ do
      let words            = Vector.fromList ["ab", "cd", "ef"]
      let grid             = make_square_grid 2 words
      let Grid nodes clues = grid
      let empty_nodes = Vector.fromList
            [ make_node_with_words 0 [] 0 [] [] words
            , make_node_with_words 0 [] 1 [] [] words
            , make_node_with_words 1 [] 0 [] [] words
            , make_node_with_words 1 [] 1 [] [] words
            ]
      find_stable_grid grid `shouldBe` Grid empty_nodes clues

    it "should handle larger lists" $ do
      let chars = ['a' .. 'j']
      let words = Vector.fromList
            [ [a, b, c, d] | a <- chars, b <- chars, c <- chars, d <- chars ]
      let Grid nodes clues         = find_stable_grid (make_square_grid 4 words)
      let Node _ res_words _ _ _ _ = Vector.head nodes
      -- words has 10k words 
      res_words `shouldBe` IntSet.fromDistinctAscList
        (take (Vector.length words) [0 ..])
