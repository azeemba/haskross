# haskross

Crossword generator in Haskell using a SMT solver

Still a work in progress!

The [SBV](https://github.com/LeventErkok/sbv) library is used as the SMT
solver which uses z3 as the default engine. To run this, z3 is expected to be
available in the local directory or in the path.
Z3 can be downloaded from their [releases](https://github.com/Z3Prover/z3/releases) page.

## Status

`src/Lib.hs` has the initial constraints with `test/Spec.hs` exercising the
constraints. The current implementation is very flexible as it tries to 
generate the shape and the words of the crossword at the same time.
This results in an excessive number of constraints which hurts performance.

This implementation was reimplemented using the C++ api here: [hackross](https://github.com/azeemba/hackross/)
