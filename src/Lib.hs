module Lib
    ( Node(Node),
      Words,
      WhichClue(First, Second),
      Grid(Grid),
      make_square_grid,
      find_stable_grid,
      iterate_on_grid,
      merge_clues,
      fix_compress,
      compress_node,
      filter_chars,
      does_char_fit,
      does_word_fit
    ) where

import Data.List
import qualified Data.Vector as Vector
import qualified Data.IntSet as IntSet

data Node = Node Int Words Int Words [Char] AllWords | NodeEmpty deriving (Show, Eq)

type Words = IntSet.IntSet
type AllWords = Vector.Vector String

data WhichClue = First | Second deriving (Enum, Eq, Show)

-- Todo: 
-- * Figure out how we go from stable state to unique state

type Clue = (WhichClue, [Int])
data Grid = Grid (Vector.Vector Node) [Clue] deriving (Eq)
instance Show Grid where
  show (Grid nodes clues) =
      Vector.foldr (\node output -> if (length output > 100) then output else output ++ show node) "" nodes



find_stable_grid :: Grid -> Grid
find_stable_grid = find_stable iterate_on_grid (Grid (Vector.fromList []) [])

iterate_on_grid :: Grid -> Grid
iterate_on_grid (Grid nodes clues) =
  let nodes'  = Vector.map fix_compress nodes
      nodes'' = foldr (\clue in_nodes -> find_and_merge_clue clue in_nodes)
                      nodes'
                      clues
  in  Grid nodes'' clues


find_and_merge_clue :: Clue -> Vector.Vector Node -> Vector.Vector Node
find_and_merge_clue (which, indices) nodes =
  let specific_nodes  = map (\ind -> nodes Vector.! ind) indices
      specific_nodes' = merge_clues which specific_nodes
  in  Vector.unsafeUpdate nodes (Vector.fromList (zip indices specific_nodes'))

--   0  1  2  3
-- 0 0  1  2  3
-- 1 4  5  6  7
-- 2 8  9  10 11
-- 3 12 13 14 15
ind2pos :: Int -> Int -> (Int, Int)
ind2pos i n = quotRem i n

make_square_grid :: Int -> AllWords -> Grid
make_square_grid size words =
  let nodes =
        map (\i -> make_square_node i size words) (take (size * size) [0 ..])
      clues =
        map (\i -> (First, take size [size * i ..])) [0 .. size - 1]
          ++ map (\i -> (Second, take size [i, i + size ..])) [0 .. size - 1]
  in  Grid (Vector.fromList nodes) clues

make_square_node :: Int -> Int -> AllWords -> Node
make_square_node i size words =
  let (x, y)  = ind2pos i size
      wordset = IntSet.fromDistinctAscList (take (Vector.length words) [0 ..])
  in  Node x wordset y wordset ['a' .. 'z'] words

-- The two nodes are part of the same clue. So we can filter out words
-- that are not in both
merge_clues :: WhichClue -> [Node] -> [Node]
merge_clues which_clue nodes =
  let words_lists = map (\node -> extract_clue (which_clue, node)) nodes
      merged      = if null words_lists
        then IntSet.empty
        else foldr1 (IntSet.intersection) words_lists
  in  map (\node -> inject_clue (which_clue, node) merged) nodes


extract_clue :: (WhichClue, Node) -> Words
extract_clue (First , (Node _ words1 _ _ _ _)) = words1
extract_clue (Second, (Node _ _ _ words2 _ _)) = words2

inject_clue :: (WhichClue, Node) -> Words -> Node
inject_clue (First, (Node ind1 _ ind2 words2 chars all)) words1 =
  Node ind1 words1 ind2 words2 chars all
inject_clue (Second, (Node ind1 words1 ind2 _ chars all)) words2 =
  Node ind1 words1 ind2 words2 chars all


find_stable :: Eq a => (a -> a) -> a -> a -> a
find_stable f base = \x' -> snd
  ( until (\(before, after) -> before == after)
          (\(before, after) -> (after, f after))
          (base, x')
  )

fix_compress :: Node -> Node
fix_compress = find_stable compress_node NodeEmpty

compress_node :: Node -> Node
compress_node (Node ind1 words1 ind2 words2 chars allWords) =
  let
    words1o = filter_words ind1 words1 chars allWords
    words2o = filter_words ind2 words2 chars allWords
    charso  = filter_chars (filter_chars chars ind1 words1o allWords)
                           ind2
                           words2o
                           allWords
  in
    Node ind1 words1o ind2 words2o charso allWords

filter_words :: Int -> Words -> [Char] -> AllWords -> Words
filter_words ind words chars allWords =
  IntSet.filter (\w -> does_word_fit_cs w ind chars allWords) words

does_word_fit_cs :: Int -> Int -> [Char] -> AllWords -> Bool
does_word_fit_cs w i cs allWords = any (\c -> does_word_fit c i allWords w) cs

filter_chars :: [Char] -> Int -> Words -> AllWords -> [Char]
filter_chars chars index words allWords =
  filter (\c -> does_char_fit c index words allWords) chars

does_char_fit :: Char -> Int -> Words -> AllWords -> Bool
does_char_fit c index words allWords = any
  id
  ( IntSet.foldr
    (\wordI answers -> (does_word_fit c index allWords wordI) : answers)
    []
    words
  )

does_word_fit :: Char -> Int -> AllWords -> Int -> Bool
does_word_fit c index allWords wordI =
  let word = allWords Vector.! wordI
  in  if index < length word then (word !! index) == c else False
