# my-tetris-solver-hs

A for fun weekend project. Inspired by last week's headache when solving the
tetris puzzles from Talos Principle by hand.

## Usage

 1. Define the puzzle in code

```haskell
problem_12 = Problem (Size 6 8) [figureI, figureT,
                                 figureT, figureSquare, figureSquare,
                                 figureS, figureTurnedS, figureTurnedS,
                                 figureTurnedS, figureTurnedL, figureTurnedL,
                                 figureTurnedL]
```

 2. Update `main`

```haskell
main = go problem_12
```

 3. Compile and run

```shell
$ ghc -O2 Tetris.hs
$ ./Tetris
EEJJHKKK
EEJHHIIK
ACJHGFII
ACCGGFFL
ACBGDDFL
ABBBDDLL
```
