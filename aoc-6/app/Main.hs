module Main (main) where

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

-- part 2
fileLines :: IO [String]
fileLines = lines <$> readFile "puzzle.txt"

colSizes :: [String] -> [Int]
colSizes ls = map (maximum . (\n -> map (!!n) lengths)) [0..length (head lengths) - 1]
  where
    lengths = map (map length . words) ls

cols :: [String] -> [Int] -> [[String]]
cols ls [] = []
cols ls (n:ns) = col : cols rest ns
  where
    col = map (take n) ls
    rest = map (drop (n+1)) ls

operateCol :: [String] -> Integer
operateCol col =
  case op of
    '*' -> product numbers
    '+' -> sum numbers
    otherwise -> error "Invalid operation"
  where
    table = init col
    getNums :: [String] -> [Integer]
    getNums xs
      | null (head xs) = []
      | otherwise = read (filter (/=' ') $ map last xs) : getNums (map init xs)
    numbers = getNums table
    op = head $ last col

part2 :: IO ()
part2 = do
  ls <- fileLines
  let cs = colSizes ls
  print $ sum $ map operateCol $ cols ls cs

main :: IO ()
main = part1
