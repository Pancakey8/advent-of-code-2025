module Main (main) where

import Text.Read
import Data.Bool (bool)

data Rotation
  = RotLeft Int
  | RotRight Int
  deriving (Show)

parseRotation :: String -> Maybe Rotation
parseRotation ('R' : xs) = RotRight <$> readMaybe xs
parseRotation ('L' : xs) = RotLeft <$> readMaybe xs
parseRotation _ = Nothing

type Dial = Int

rotateDial :: Dial -> Rotation -> Dial
rotateDial d (RotRight x) = (d + x) `mod` 100
rotateDial d (RotLeft x) = (d - x) `mod` 100

data CountedDial = Counted Dial Int
  deriving (Show)

initial :: CountedDial
initial = Counted 50 0

rotateCounted :: CountedDial -> Rotation -> CountedDial
rotateCounted (Counted d n) r = Counted d' n'
  where
    d' = rotateDial d r
    n' = if d' == 0 then n + 1 else n

countOnList :: CountedDial -> [Rotation] -> CountedDial
countOnList = foldl rotateCounted

-- Part 2 function
findZeroCross :: Dial -> Rotation -> Int
findZeroCross d (RotLeft x)
  | x >= 100 = x `div` 100 + findZeroCross d (RotLeft $ x `mod` 100)
  | otherwise = bool 0 1 (x >= d && d /= 0) 
findZeroCross d (RotRight x)
  | x >= 100 = x `div` 100 + findZeroCross d (RotRight $ x `mod` 100)
  | otherwise = bool 0 1 (x >= 100 - d && d /= 0) 

-- Part 2 function
rotateCounted' :: CountedDial -> Rotation -> CountedDial
rotateCounted' (Counted d n) r = Counted d' n'
  where
    d' = rotateDial d r
    n' = n + findZeroCross d r

-- Part 2 function
countOnList' :: CountedDial -> [Rotation] -> CountedDial
countOnList' = foldl rotateCounted'

file :: IO [String]
file = lines <$> readFile "puzzle.txt"

rotations :: IO (Maybe [Rotation])
rotations = fmap sequence $ map parseRotation <$> file

counterOnRotations :: IO (Maybe CountedDial)
counterOnRotations = fmap (countOnList' initial) <$> rotations -- countOnList for part1, countOnList' for part2

main :: IO ()
main = do
  c <- counterOnRotations
  case c of
    Just (Counted d n) -> putStrLn ("Counted landed on " ++ show d ++ " with password " ++ show n)
    Nothing -> putStrLn "Failed to parse file"
