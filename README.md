# haskross

Crossword generator in Haskell using a SMT solver

Still a work in progress!

The [SBV](https://github.com/LeventErkok/sbv) library is used as the SMT
solver which uses z3 as the default engine. To run this, z3 is expected to be
available in the local directory or in the path.
Z3 can be downloaded from their [releases](https://github.com/Z3Prover/z3/releases) page.

## Status

`src/Lib.hs` has the initial constraints with `test/Spec.hs` exercising the
constraints. For a clue based approach, these constraints are sufficient.
Soon, we will switch to a "node-value"
based approach which should be more flexible.