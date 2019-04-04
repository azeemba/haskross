module Lib
    ( someFunc,
      Node(Node),
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

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Node = Node Int Words Int Words [Char] | NodeEmpty deriving (Show, Eq)

type Words = [String]

data WhichClue = First | Second deriving (Enum, Eq, Show)

-- Todo: 
-- * Figure out how we go from stable state to unique state

type Clue = (WhichClue, [Int])
data Grid = Grid (Vector.Vector Node) [Clue] deriving (Eq, Show)

find_stable_grid :: Grid -> Grid
find_stable_grid = find_stable iterate_on_grid (Grid (Vector.fromList []) [])

iterate_on_grid :: Grid -> Grid
iterate_on_grid (Grid nodes clues) =
  let nodes' = Vector.map fix_compress nodes
      nodes'' = foldr (\clue in_nodes -> find_and_merge_clue clue in_nodes) nodes' clues
  in Grid nodes'' clues


find_and_merge_clue :: Clue -> Vector.Vector Node -> Vector.Vector Node
find_and_merge_clue (which, indices) nodes =
  let specific_nodes = map (\ind -> Vector.unsafeIndex nodes ind) indices
  in  Vector.unsafeUpdate nodes (Vector.fromList (zip indices specific_nodes))

--   0  1  2  3
-- 0 0  1  2  3
-- 1 4  5  6  7
-- 2 8  9  10 11
-- 3 12 13 14 15
ind2pos :: Int -> Int -> (Int, Int)
ind2pos i n = quotRem i n

make_square_grid :: Int -> Words -> Grid
make_square_grid size words =
  let nodes =
        map (\i -> make_square_node i size words) (take (size * size) [0 ..])
      clues =
        map (\i -> (First, take size [size * i ..])) [0 .. size - 1]
          ++ map (\i -> (Second, take size [i, i + size ..])) [0 .. size - 1]
  in  Grid (Vector.fromList nodes) clues

make_square_node :: Int -> Int -> Words -> Node
make_square_node i size words =
  let (x, y) = ind2pos i size in Node x words y words ['a' .. 'z']

-- The two nodes are part of the same clue. So we can filter out words
-- that are not in both
merge_clues :: WhichClue -> [Node] -> [Node]
merge_clues which_clue nodes =
  let words_lists = map (\node -> extract_clue (which_clue, node)) nodes
      merged = if null words_lists then [] else foldr1 intersect words_lists
  in  map (\node -> inject_clue (which_clue, node) merged) nodes


extract_clue :: (WhichClue, Node) -> Words
extract_clue (First , (Node _ words1 _ _ _)) = words1
extract_clue (Second, (Node _ _ _ words2 _)) = words2

inject_clue :: (WhichClue, Node) -> Words -> Node
inject_clue (First, (Node ind1 _ ind2 words2 chars)) words1 =
  Node ind1 words1 ind2 words2 chars
inject_clue (Second, (Node ind1 words1 ind2 _ chars)) words2 =
  Node ind1 words1 ind2 words2 chars


find_stable :: Eq a => (a -> a) -> a -> a -> a
find_stable f base = \x' -> snd
  ( until (\(before, after) -> before == after)
          (\(before, after) -> (after, f after))
          (base, x')
  )

fix_compress :: Node -> Node
fix_compress = find_stable compress_node NodeEmpty

compress_node :: Node -> Node
compress_node (Node ind1 words1 ind2 words2 chars) =
  let words1o = filter_words ind1 words1 chars
      words2o = filter_words ind2 words2 chars
      charso  = filter_chars (filter_chars chars ind1 words1o) ind2 words2o
  in  Node ind1 words1o ind2 words2o charso

filter_words :: Int -> Words -> [Char] -> Words
filter_words ind words chars =
  filter (\w -> does_word_fit_cs w ind chars) words

does_word_fit_cs :: String -> Int -> [Char] -> Bool
does_word_fit_cs w i cs = any (\c -> does_word_fit c i w) cs

filter_chars :: [Char] -> Int -> Words -> [Char]
filter_chars chars index words =
  filter (\c -> does_char_fit c index words) chars

does_char_fit :: Char -> Int -> Words -> Bool
does_char_fit c index words = any (does_word_fit c index) words

does_word_fit :: Char -> Int -> String -> Bool
does_word_fit c index word =
  if index < length word then (word !! index) == c else False
