module Main (main) where
import Data.Char (isSpace)
import Data.List (transpose)

-- part 1 
readGrid :: IO [[String]]
readGrid = map words . lines <$> readFile "puzzle.txt"

columnOp :: [String] -> Integer
columnOp xs =
  case last xs of
    "+" -> sum $ map read $ init xs
    "*" -> product $ map read $ init xs
    _ -> error "Invalid operation"

grandSum :: [[String]] -> Integer
grandSum xs = sum $ map (columnOp . (\n -> map (!!n) xs)) [0..length (head xs) - 1]

part1 :: IO ()
part1 = print . grandSum =<< readGrid

-- part 2, transpose
cols :: [String] -> [[String]]
cols [] = []
cols xs = takeWhile (not . isBlank) xs : cols (tailSafe $ dropWhile (not . isBlank) xs)
  where
    tailSafe [] = []
    tailSafe xs = tail xs
    isBlank = all isSpace

part2 :: IO ()
part2 = do
  ls <- lines <$> readFile "puzzle.txt"
  let ops = map (: []) $ words $ last ls
  let cs = cols $ transpose $ init ls
  print $ sum $ map columnOp $ zipWith (++) cs ops

main :: IO ()
main = part1
