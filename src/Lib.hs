module Lib
    ( someFunc,
      Node(Node),
      Words,
      fix_compress,
      compress_node,
      filter_chars,
      does_char_fit,
      does_word_fit
    ) where

import Control.Monad.Fix
someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Node = Node Int Words Int Words [Char] | NodeEmpty deriving (Show, Eq)

type Words = [String]

fix_compress :: Node -> Node
fix_compress = \node -> snd
  ( until (\(before, after) -> before == after)
          (\(before, after) -> (after, compress_node after))
          (NodeEmpty, node)
  )

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
