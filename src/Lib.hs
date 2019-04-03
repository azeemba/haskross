module Lib
    ( someFunc,
      Node(Node),
      Words,
      WhichClue(First, Second),
      merge_clues,
      fix_compress,
      compress_node,
      filter_chars,
      does_char_fit,
      does_word_fit
    ) where

import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Node = Node Int Words Int Words [Char] | NodeEmpty deriving (Show, Eq)

type Words = [String]

data WhichClue = First | Second deriving (Enum)

-- The two nodes are part of the same clue. So we can filter out words
-- that are not in both
merge_clues :: (WhichClue, Node) -> (WhichClue, Node) -> (Node, Node)
merge_clues clue_pair1 clue_pair2 =
  let words1 = extract_clue clue_pair1
      words2 = extract_clue clue_pair2
      merged = words1 `intersect` words2
      node1 = inject_clue clue_pair1 merged
      node2 = inject_clue clue_pair2 merged
  in (node1, node2)


extract_clue :: (WhichClue, Node) -> Words
extract_clue (First, (Node _ words1 _ _ _)) =  words1
extract_clue (Second, (Node _  _ _ words2 _)) =  words2

inject_clue :: (WhichClue, Node) -> Words -> Node
inject_clue (First, (Node ind1  _ ind2 words2 chars)) words1 = Node ind1 words1 ind2 words2 chars
inject_clue (Second, (Node ind1 words1 ind2 _ chars)) words2 = Node ind1 words1 ind2 words2 chars


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
