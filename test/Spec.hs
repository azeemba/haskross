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

  describe "constrain_by_clue" $ do
    it "should pick the sole value" $ do
      result <- allSat $ do
        xs <- sChar "a"
        let nodes     = Vector.fromList [xs]
        let clue      = [0]
        let all_words = ["a"]
        constrain $ constrain_by_clue clue nodes all_words
      let xs = getModelValues "a" result
      catMaybes xs `shouldMatchList` ['a']
    it "should handle clue intersections" $ do
      let node_names = ["1", "2", "3"]
      result <- allSat $ do
        xs <- sChars node_names
        let nodes     = Vector.fromList xs
        let clue1     = [0, 1, 2]
        let clue2     = [0] -- that's not how intersecting clues line up but whatever
        let allWords1 = ["cat", "bag"]
        let allWords2 = ["c", "p", "q"]
        constrain $ constrain_by_clue clue1 nodes allWords1
        constrain $ constrain_by_clue clue2 nodes allWords2
      let values = map
            (\name -> head . catMaybes $ getModelValues name result)
            node_names
      values `shouldBe` ['c', 'a', 't']
