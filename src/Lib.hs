module Lib
    ( someFunc,
      Node(Node),
      Words,
      filter_node,
      filter_chars,
      does_char_fit,
      does_word_fit
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Node = Node [Char] Int Int deriving (Show, Eq)
type Words = [String]

filter_node :: Node -> Words -> Node
filter_node (Node chars row col) words = Node filtered_chars row col
    where filtered_chars = filter_chars (filter_chars chars row words) col words

filter_chars :: [Char] -> Int -> Words -> [Char]
filter_chars chars index words = filter (\c -> does_char_fit c index words) chars

does_char_fit :: Char -> Int -> Words -> Bool
does_char_fit c index words = any (does_word_fit c index) words

does_word_fit :: Char -> Int -> String -> Bool
does_word_fit c index word = if index < length word
    then (word !! index) == c
    else False