import Test.Hspec
import qualified Data.Vector as Vector
import Data.Maybe
import Control.Exception (evaluate)
import Data.List
import Data.SBV

import Lib


main :: IO ()
main = hspec $ do
  describe "constrain_node_letters" $ do
    it "should pick the sole value" $ do
      result <- allSat $ do
        x <- exists "x"
        constrain $ constrain_node_letters x ['x']
      let x = getModelValues "x" result
      catMaybes x `shouldBe` ['x']
    it "should fail with no match" $ do
      result <- sat $ do
        x <- exists "x"
        constrain $ constrain_node_letters x []
      modelExists result `shouldBe` False
    it "should allow multiple matches" $ do
      result <- allSat $ do
        x <- exists "x"
        constrain $ constrain_node_letters x ['a', 'b', 'c']
      let xs = getModelValues "x" result
      catMaybes xs `shouldMatchList` ['a', 'b', 'c']

  describe "constrain_seq_by_word_at_pos" $ do
    it "should pick the sole value" $ do
      let node_names = ["0", "1", "2"]
      result <- allSat $ do
        xs <- sChars node_names
        let nodes = Vector.fromList xs
        let word  = "cat"
        constrain $ constrain_seq_by_word_at_pos [0, 1, 2] nodes word
      let values = map
            (\name -> head . catMaybes $ getModelValues name result)
            node_names
      values `shouldBe` ['c', 'a', 't']
    it "should fail with no match with inconsistent condition" $ do
      let node_names = ["0", "1", "2"]
      result <- sat $ do
        xs <- sChars node_names
        let nodes = Vector.fromList xs
        constrain $ constrain_seq_by_word_at_pos [0, 1, 2] nodes "cat"
        constrain $ constrain_seq_by_word_at_pos [1] nodes "z"
      modelExists result `shouldBe` False
    it "should handle valid intersections" $ do
      let node_names = ["0", "1", "2"]
      result <- allSat $ do
        xs <- sChars node_names
        let nodes = Vector.fromList xs
        let word  = "cat"
        constrain $ constrain_seq_by_word_at_pos [0, 1, 2] nodes word
        constrain $ constrain_seq_by_word_at_pos [1] nodes "a"
      let values = map
            (\name -> head . catMaybes $ getModelValues name result)
            node_names
      values `shouldBe` ['c', 'a', 't']

  describe "constrain_seq_by_word" $ do
    it "should set the word in all possible positions" $ do
      let node_names = ["0", "1", "2", "3"]
      result <- allSat $ do
        xs <- sChars node_names
        let nodes = Vector.fromList xs
        let word  = "at"
        constrain
          $ bAll (\x -> constrain_node_letters x ('#' : ['a' .. 'z'])) xs
        constrain $ constrain_seq_by_word (take 4 [0 ..]) nodes word
      let result_strings = solution2strings result node_names
      let unexpected_strings =
            filter (\word -> not $ "#at" `isInfixOf` word)
              $ filter (\word -> not $ "at#" `isInfixOf` word) result_strings
      unexpected_strings `shouldBe` []


-- Takes the solution of each node_name and puts them together in the same
-- order. So ['a','b'] and ['c', 'd'] solutions become ["ac", "bd"]
solution2strings :: AllSatResult -> [String] -> [String]
solution2strings result node_names =
  let node_values :: [String]
      node_values =
        map (\name -> catMaybes $ getModelValues name result) node_names
      num_solutions      = length (head node_values)
      starting_solutions = take num_solutions (repeat "")
      add_to_solutions values solutions =
        map (\(value, result) -> value : result) (zip values solutions)
      result_string = foldr add_to_solutions starting_solutions node_values
  in  result_string
