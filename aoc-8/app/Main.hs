module Main (main) where

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import Data.UnionFind.IO hiding (Point)
import Data.Ord (comparing, Down (..))
import Data.List (sortBy, sortOn)
import qualified Data.UnionFind.IO as UF

type Point = (Int, Int, Int)

dist3 :: Point -> Point -> Int
dist3 (x1,y1,z1) (x2,y2,z2) = (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn p s = case dropWhile (==p) s of
                [] -> []
                s' -> let (w, s'') = break (==p) s'
                      in w : splitOn p s''

readPoints :: IO (V.Vector Point)
readPoints = V.fromList . map (\x -> case splitOn ',' x of
                          [a,b,c] -> (read a, read b, read c)
                          _ -> error "Invalid point") . lines <$> readFile "puzzle.txt"

allPairs :: V.Vector Point -> [(Int, Int, Int)] -- Distance, i, j
allPairs ps = [ (dist3 (ps V.! i) (ps V.! j), i, j) | i <- [0 .. n], j <- [i+1 .. n] ]
              where n = V.length ps - 1

part1 :: IO ()
part1 = do
  points <- readPoints
  let pairs = take 1000 $ sortOn (\(d,_,_) -> d) $ allPairs points -- 1000 pairs
  uf <- mapM fresh points -- create union-find, each point disjoint
  mapM_ (\(_, i, j) -> (uf V.! i) `union` (uf V.! j)) pairs -- connect pairs
  reprs <- mapM descriptor uf
  let sorted = V.modify VA.sort reprs
  print $ product $ take 3 $ -- and take biggest 3, product, print
    -- sort first, groups identicals, then sort again by length
    sortBy (comparing Data.Ord.Down) (map length $ V.group sorted)

unionDifferent :: Eq a => UF.Point a -> UF.Point a -> IO Bool
unionDifferent a b = do
  ra <- descriptor a
  rb <- descriptor b
  if ra /= rb
    then union a b >> return True
    else return False

allConnected :: Eq a => [UF.Point a] -> IO Bool
allConnected uf = do
  reps <- mapM descriptor uf
  let fs = head reps
  return $ all (== fs) reps

part2 :: IO ()
part2 = do
  points <- readPoints
  let pairs = sortOn (\(d,_,_) -> d) $ allPairs points -- all pairs
  uf <- V.mapM fresh points -- union-find again
  let go [] lastPair = return lastPair
      go ((_, i, j):rest) lastPair = do
        m <- unionDifferent (uf V.! i) (uf V.! j) -- try-union if not joined
        if m
          then do -- if union
            conn <- allConnected $ V.toList uf -- check if all connected
            if conn
               -- if we're all connected after unioning this pair, then this pair was last
               then return $ Just (points V.! i, points V.! j)
               -- not all connected, keep going
               else go rest $ Just (points V.! i, points V.! j)
          else go rest lastPair -- didn't union, just keep running
  ans <- go pairs Nothing -- run on all pairs
  case ans of
    Just ((x1, _, _), (x2, _, _)) -> print $ x1 * x2
    Nothing -> print "No solution"

main :: IO ()
main = part2
