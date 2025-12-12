module Main where

type Shape = [String]
data Tree = Tree Int Int [Int]
          deriving (Show)

getFile :: IO ([Shape], [Tree])
getFile = do
  file <- lines <$> readFile "puzzle.txt"
  let (file', shapes) = foldl (\(f, ss) _ -> let (f', s) = getShape f
                                    in (f', ss ++ [s])) (file, []) [0..5] -- !! Too lazy to parse properly
                                                                -- ^^^^^^    Change to match your incides
      trees = map getTree file'
  return (shapes, trees)
  where
    getShape f = (drop 5 f, take 3 $ tail f)
    getTree l = let (dim, (_:is)) = break (==':') l
                    (x, (_:y)) = break (=='x') dim
                in Tree (read x) (read y) (map read $ words is)

-- | Prune based on # of tiles vs. the total space
-- Also this just works for my input
isFitting :: [Int] -> Tree -> Bool
isFitting ss (Tree w h is) = w * h >= sum (zipWith (*) is ss)

part1 :: IO ()
part1 = do
  (shapes, trees) <- getFile
  let cs = map (length . filter (=='#') . unlines) shapes
      trees' = filter (isFitting cs) trees
  print $ length trees'

main :: IO ()
main = putStrLn "Hello, Haskell!"
