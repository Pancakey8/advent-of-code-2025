module Main (main) where

import Data.List (sortOn)

-- import Data.Set qualified as S

splitOn :: (Eq a) => (a -> Bool) -> [a] -> [[a]]
splitOn p s = case dropWhile p s of
  [] -> []
  s' -> w : splitOn p s''
    where
      (w, s'') = break p s'

type Range = (Integer, Integer)

ranges :: [String] -> [Range]
ranges = map ((\[a, b] -> (read a, read b)) . splitOn ('-' ==))

anyRange :: [Range] -> Integer -> Bool
anyRange [] _ = False
anyRange ((a, b) : rs) n = (a <= n && n <= b) || anyRange rs n

findInRange :: [Integer] -> [Range] -> [Integer]
findInRange n rs = filter (anyRange rs) n

parseFile :: IO ([Range], [String])
parseFile = (\file -> (ranges $ head file, last file)) <$> fileIO
  where
    fileIO = splitOn (== "") . lines <$> readFile "puzzle.txt"

answer1 :: [Range] -> [String] -> Int
answer1 rs ns = length $ findInRange (map read ns) rs

mergeRanges :: [Range] -> [Range]
mergeRanges rs = merge (sortOn fst rs) []
  where
    merge [] acc = reverse acc
    merge (r : rs') [] = merge rs' [r]
    merge ((s, e) : rs') acc@((as, ae) : rest)
      | s <= ae + 1 = merge rs' ((as, max e ae) : rest)
      | otherwise = merge rs' ((s, e) : acc)

answer2 :: [Range] -> Integer
answer2 = sum . map (\(a, b) -> b - a + 1) . mergeRanges

main :: IO ()
main = do
  (set, ns) <- parseFile
  print $ answer2 set
