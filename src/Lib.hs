module Lib
    ( NodeValue,
      Clue,
      Nodes,
      Grid(..),
      constrain_node_letters,
      constrain_by_clue
    ) where

import Data.List
import Data.SBV
import qualified Data.Vector as Vector


type NodeValue = SChar
type Position = (Int, Int)
type Clue = [Int]

type Nodes = Vector.Vector NodeValue
data Grid = Grid Nodes [Clue]
ind2pos :: Int -> Int -> Position
ind2pos = quotRem

type PossibleWords = [String]
type ClueValue = SInt32

constrain_node_letters :: NodeValue -> [Char] -> SBool
constrain_node_letters node chars = bAny (\c -> literal c .== node) chars

constrain_by_clue :: Clue -> Nodes -> PossibleWords -> SBool
constrain_by_clue clue nodes all_words =
  let node_ind_pairs = zip (map (\pos -> nodes Vector.! pos) clue) [0 ..]
      all_nodes_fit word =
        bAll (\(node, ind) -> literal (word !! ind) .== node) node_ind_pairs
  in  bAny all_nodes_fit all_words

