# haskross

A Crossword generator in Haskell

Still a work in progress!

`src/Lib.hs` can start with a word list and get to a "superposition" state of all
possible words for each clue that will allow for a consistent result.

Next step is to pick one of the superposition states randomly to end up with one state.