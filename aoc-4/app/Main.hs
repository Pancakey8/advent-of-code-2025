module Main (main) where

import Data.Array.Unboxed
import Data.Set qualified as S

type Coord = (Int, Int)

type Grid = UArray Coord Bool

gridSize :: Int
gridSize = 136

deltas :: [Coord]
deltas =
  [ (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1)
  ]

neighbors :: Coord -> [Coord]
neighbors (r, c) =
  [ (r + dr, c + dc)
  | (dr, dc) <- deltas,
    let r' = r + dr,
    let c' = c + dc,
    r' >= 0,
    r' < gridSize,
    c' >= 0,
    c' < gridSize
  ]

neighborCount :: Grid -> Coord -> Int
neighborCount g ix =
  length [() | j <- neighbors ix, g ! j]

inputGrid :: IO Grid
inputGrid = do
  raw <- readFile "puzzle.txt"
  let rows = lines raw
      assocs =
        [ ((r, c), ch == '@')
        | (r, row) <- zip [0 ..] rows,
          (c, ch) <- zip [0 ..] row
        ]
  return (array ((0, 0), (gridSize - 1, gridSize - 1)) assocs)

aliveSet :: Grid -> S.Set Coord
aliveSet g =
  S.fromList [ix | ix <- indices g, g ! ix]

accessibleIn :: Grid -> S.Set Coord -> S.Set Coord
accessibleIn g = S.filter (\ix -> g ! ix && neighborCount g ix < 4)

removeCells :: Grid -> S.Set Coord -> Grid
removeCells g removed =
  let b = bounds g
   in array
        b
        [ (ix, (g ! ix) && ix `S.notMember` removed)
        | ix <- range b
        ]

step :: Grid -> S.Set Coord -> S.Set Coord -> (Grid, S.Set Coord)
step g alive boundary =
  let candidates = boundary `S.intersection` alive
      acc = accessibleIn g candidates
   in if S.null acc
        then (g, S.empty)
        else
          let g' = removeCells g acc
              alive' = alive `S.difference` acc
              neigh = S.fromList (concatMap neighbors (S.toList acc))
              boundary' = neigh `S.intersection` alive'
           in (g', boundary')

iterateRemoval :: Grid -> Int
iterateRemoval g0 =
  let alive0 = aliveSet g0
      boundary0 = alive0
      loop g alive boundary accCount =
        let (g', boundary') = step g alive boundary
            removed = alive `S.difference` aliveSet g'
            k = S.size removed
         in if k == 0
              then accCount
              else loop g' (alive `S.difference` removed) boundary' (accCount + k)
   in loop g0 alive0 boundary0 0

main :: IO ()
main = print . iterateRemoval =<< inputGrid
