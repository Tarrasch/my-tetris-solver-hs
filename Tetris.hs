#!/usr/bin/env stack
-- stack --resolver lts-9.13 script

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Char (ord, chr)
import Data.List (foldl', maximumBy, nub, scanl')
import Data.Maybe (fromJust, fromMaybe)
import Control.Monad (msum, guard)
import Data.Ord
import Data.Function
import qualified Data.Set as Set

data Size = Size Int Int  -- y and x
  deriving (Eq, Show)
data Coord = Coord Int Int  -- y and x
  deriving (Eq, Show, Ord)
newtype Figure = Figure (Set Coord) -- Should always be 4 elements
  deriving (Eq, Show)
type Mapping = Map Coord Int  -- (coord -> ix of figure there)
data Board = Board Size Mapping
  deriving (Eq, Show)
data State = State Board [[Figure]] -- remaining figures
   deriving (Eq, Show)
data Problem = Problem Size [Figure]
  deriving (Eq, Show)
type Solution = Board  -- Altough it should be fully covered

emptyBoard size = Board size Map.empty

mkFigure (y0, x0) (y1, x1) (y2, x2) (y3, x3) =
        Figure $ Set.fromList [
                Coord y0 x0,
                Coord y1 x1,
                Coord y2 x2,
                Coord y3 x3
                ]

figureSquare = mkFigure (0,0) (0,1) (1,0) (1,1)
figureL = mkFigure (0,0) (1,0) (2,0) (0,1)
figureTurnedL = mkFigure (0,0) (1,0) (2,0) (2,1)
figureI = mkFigure (0,0) (1,0) (2,0) (3,0)
figureS = mkFigure (0,0) (0,1) (1,1) (1,2)
figureTurnedS = mkFigure (0,0) (1,0) (1,1) (2,1)
figureT = mkFigure (0, 0) (0, 1) (1, 1) (0, 2)

problem_1  = Problem (Size 2 2) [figureSquare]
problem_2  = Problem (Size 4 4) (replicate 4 figureSquare)
problem_3  = Problem (Size 6 6) (replicate 9 figureSquare)
problem_4  = Problem (Size 8 8) (replicate 16 figureSquare)
problem_5  = Problem (Size 2 4) (replicate 2 figureL)
problem_6  = Problem (Size 6 4) (replicate 6 figureI)
problem_7  = Problem (Size 3 4) [figureS, figureL, figureTurnedL]
problem_8  = Problem (Size 5 4) [figureT, figureT, figureI, figureTurnedS, figureL]
problem_9  = Problem (Size 6 8) [figureT, figureT, figureSquare,
                                 figureSquare, figureI, figureL,
                                 figureL, figureTurnedL, figureTurnedL,
                                 figureTurnedL, figureS, figureS]
problem_10 = Problem (Size 10 4) [figureT, figureT,
                                  figureT, figureT,
                                  figureTurnedS, figureTurnedS,
                                  figureS, figureI,
                                  figureI, figureTurnedL]
problem_11 = Problem (Size 4 7) [figureI, figureI,
                                 figureT, figureT,
                                 figureTurnedS, figureS,
                                 figureL]
problem_12 = Problem (Size 6 8) [figureI, figureT,
                                 figureT, figureSquare, figureSquare,
                                 figureS, figureTurnedS, figureTurnedS,
                                 figureTurnedS, figureTurnedL, figureTurnedL,
                                 figureTurnedL]


solveProblem :: Problem -> Maybe Solution
solveProblem = solveLoop 0 . initialState

initialState :: Problem -> State
initialState (Problem sz figures) = State (emptyBoard sz) (map (rps sz) figures)

solveLoop :: Int -> State -> Maybe Solution
solveLoop round (State board [])                         = Just board
solveLoop round (State board (placements_:placementss')) = msum $ do
    placement <- placements_
    guard $ all (not . flip Map.member mapping) (coords placement)
    let insertRound = \m k -> Map.insert k round m
    let newMapping = foldl' insertRound mapping (coords placement)
    let newBoard = Board sz newMapping
    guard $ floodFillCheck newBoard
    return $ solveLoop (round + 1) (State newBoard placementss')
  where
    (Board sz mapping) = board

row (Coord y _) = y
col (Coord _ x) = x
incBy y x (Coord y0 x0) = Coord (y+y0) (x+x0)
maxY figure = maximum (Set.map row $ coords figure)
maxX figure = maximum (Set.map col $ coords figure)
minY figure = minimum (Set.map row $ coords figure)
minX figure = minimum (Set.map col $ coords figure)
rotf1 = id
rotf2 (Coord y x) = (Coord (-x) y)
rotf3 (Coord y x) = (Coord (-y) (-x))
rotf4 (Coord y x) = (Coord x (-y))
rotfs = [rotf1, rotf2, rotf3, rotf4]

rotations figure0 = nub [Figure (Set.map (incFunction figure) (coords figure)) | figure <- rots]
  where incFunction figure = incBy (negate $ minY figure) (negate $ minX figure)
        rots = [Figure (Set.map rotf (coords figure0)) | rotf <- rotfs]
placements (Size n m) figure = [Figure (Set.map f (coords figure)) | f <- incFunctions]
  where incFunctions = [incBy y x | y <- [0..maxN], x <- [0..maxM]]
        maxN = n - 1 - maxY figure
        maxM = m - 1 - maxX figure
coords (Figure coords_) = coords_
rps sz figure = do
    rotation <- rotations figure
    placement <- placements sz rotation
    return placement

formatSolution :: Solution -> String
formatSolution (Board (Size n m) mapping) = unlines allLines
  where
    allLines = reverse [formatLine y | y <- [0..n-1]]
    formatLine y = [charAt y x | x <- [0..m-1]]
    chrFromZero val = chr $ (ord 'A') + val
    charAt y x = chrFromZero $ fromJust $ Map.lookup (Coord y x) mapping

floodFillCheck :: Board -> Bool
floodFillCheck (Board sz mapping0) = all divisibleBy4 (map snd subSteps)
  where foldf (mapping, _num) coord = floodFill sz mapping coord
        subSteps = scanl' foldf (mapping0, 0) (allCoords sz)
        divisibleBy4 = (== 0) . (`mod` 4)

allCoords (Size n m) = [Coord y x | y <- [0..n-1], x <- [0..m-1]]
inBound (Size n m) (Coord y x) = 0 <= y && y < n && 0 <= x && x < m
neighbours (Coord y x) = [Coord (y+1) x, Coord y (x+1), Coord (y-1) x, Coord y (x-1)]

floodFill :: Size -> Mapping -> Coord -> (Mapping, Int)
floodFill sz mapping0 coord0 | not isOk  = (mapping0, 0)
                             | otherwise = foldl' foldf (Map.insert coord0 (-1) mapping0, 1) (neighbours coord0)
  where foldf (mapping, num) coord = fmap (+num) $ floodFill sz mapping coord
        isOk = inBound sz coord0 && (not $ Map.member coord0 mapping0)

humanSolve = fromMaybe "No solution :(" . fmap formatSolution . solveProblem
go = putStrLn . humanSolve

main = go problem_12
