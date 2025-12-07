module Main (main) where
import Data.List (elemIndex, elemIndices)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import qualified Data.Set as S
import Data.Function ((&))

type Map = [String]
type Beam = (Int, Int) -- Row, Col

data State = State { manifold :: Map, beams :: S.Set Beam, splits :: Int }
  deriving (Show)

iterateOnce :: State -> State
iterateOnce State { manifold = m, beams = bs, splits = s } =
    State { manifold = m, beams = bs', splits = s' }
  where
    h = length m
    w = length $ head m

    step (r, c)
      | r+1 >= h = (S.empty, 0)
      | otherwise =
          case m !! r !! c of
            '^' ->
              let kids = filter (\(_, c') -> 0 <= c' && c < w) [(r+1, c-1), (r+1, c+1)]
              in (S.fromList kids, 1)
            _ ->
              if 0 <= c && c < w
              then (S.singleton (r+1, c), 0)
              else (S.empty, 0)

    results = map step (S.toList bs)
    bs' = S.unions (map fst results)
    s'= s + sum (map snd results)

play :: State -> State
play s
  | null $ beams s = s
  | otherwise = play $ iterateOnce s

part1 :: IO ()
part1 = do
  m <- lines <$> readFile "puzzle.txt"
  let state = State { manifold = m,
                      beams = S.fromList [(0, fromJust (elemIndex 'S' $ head m))],
                      splits = 0 }
  print $ splits $ play state

listAdd :: Num a => Int -> a -> [a] -> [a]
listAdd i n ns
  | 0 <= i && i < length ns = take i ns ++ [ns !! i + n] ++ drop (i+1) ns
  | otherwise = ns

countTimelines :: Map -> Int
countTimelines rs = sum $ foldl nextRow start $ tail rs
  where
    h = length rs
    w = length $ head rs

    cs = fromJust $ elemIndex 'S' $ head rs
    start = [if c == cs then 1 else 0 | c <- [0..w-1]]

    nulls = replicate w 0

    nextRow counts row = foldl (iter row) nulls $ zip [0..] counts

    iter row prev (col, cnt)
      | cnt == 0 = prev
      | row !! col == '^' = listAdd (col - 1) cnt $ listAdd (col + 1) cnt prev
      | otherwise = listAdd col cnt prev

part2 :: IO ()
part2 = do
  m <- lines <$> readFile "puzzle.txt"
  print $ countTimelines m

main :: IO ()
main = part2
