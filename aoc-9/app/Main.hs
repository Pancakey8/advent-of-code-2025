{-# LANGUAGE LambdaCase #-}
module Main (main) where
import           Data.List (tails)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn p s = case dropWhile (==p) s of
                [] -> []
                s' -> let (w, s'') = break (==p) s'
                      in w : splitOn p s''

type Point = (Int, Int)

area :: Point -> Point -> Int
area (a, b) (c, d) = (abs (c - a) + 1) * (abs (d - b) + 1)

getPoints :: IO [Point]
getPoints = map ((\case
                     [a,b] -> (read a, read b)
                     _ -> error "Invalid pair") . splitOn ',') . lines <$> readFile "puzzle.txt"


choose2 :: [a] -> [(a, a)]
choose2 l = [(x, y) | (x:ys) <- tails l, y <- ys]

part1 :: IO ()
part1 = do
  points <- getPoints
  let pairs = choose2 points
      areas = [ area a b | (a,b) <- pairs ]
  print $ maximum areas

data Rect = Rect Point Point -- (x1, y1) (x2, y2)

contained :: Rect -> [Point] -> Bool
contained (Rect (x, y) (x', y')) = all inside . edges
  where
    edges ps = zip ps $ drop 1 $ cycle ps
    inside ((x1, y1), (x2, y2)) =
         (max x1 x2 <= min x x') -- edge on left
      || (max y1 y2 <= min y y') -- edge on top
      || (max x x' <= min x1 x2) -- edge on right
      || (max y y' <= min y1 y2) -- edge on bottom

part2 :: IO ()
part2 = do
  points <- getPoints
  let pairs = choose2 points
      areas = [ area a b | (a, b) <- pairs, contained (Rect a b) points ]
  print $ maximum areas

main :: IO ()
main = part2
