module Lib
    ( NodeValue,
      Clue,
      Nodes,
      Grid(..),
      constrain_node_letters,
      constrain_seq_by_words,
      constrain_seq_by_word,
      constrain_seq_by_word_at_pos
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

-- constrain_by_words :: Int -> Nodes -> PossibleWords -> SBool

constrain_seq_by_words :: [Int] -> Nodes -> PossibleWords -> SBool
constrain_seq_by_words indices all_nodes all_words =
  bAny (constrain_seq_by_word indices all_nodes) all_words

-- size : 5
-- word size : 2
-- 0  1  2  3  4
-- [0, 1, 2, 3]
-- [1, 2, 3, 4]
-- unique_pos_counts = 2
-- = size - (word_size + 2) + 1
-- Also add: [0, 1, 2] and [2, 3, 4]
constrain_seq_by_word :: [Int] -> Nodes -> String -> SBool
constrain_seq_by_word indices all_nodes word =
  let
    size              = length indices
    wrapped_word      = "#" ++ word ++ "#"
    word_len          = 2 + length word
    unique_pos_counts = size - word_len + 1
    word_spots        = map (\nth -> take word_len $ drop nth indices)
                            (take unique_pos_counts [0 ..])
    inner_constraints = bAny (\is -> constrain_seq_by_word_at_pos is all_nodes wrapped_word) word_spots
    left_constraint = constrain_seq_by_word_at_pos (take (length word + 1) [0..]) all_nodes (word ++ "#")
    right_constraint = constrain_seq_by_word_at_pos (reverse $ take (length word + 1) [size-1, size-2..]) all_nodes ("#" ++ word)
  in bOr [left_constraint, inner_constraints, right_constraint]

constrain_seq_by_word_at_pos :: [Int] -> Nodes -> String -> SBool
constrain_seq_by_word_at_pos indices all_nodes word =
  let nodes_list = map (\ind -> all_nodes Vector.! ind) indices
      node_ind_pairs = zip nodes_list [0 ..]
  in  bAll (\(node, ind) -> literal (word !! ind) .== node) node_ind_pairs
