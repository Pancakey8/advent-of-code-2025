module Main (main) where

wordsOn :: (Char -> Bool) -> String -> [String]
wordsOn p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsOn p s''
    where
      (w, s'') = break p s'

newtype Range = Range (Int, Int)
  deriving (Show)

parseRange :: String -> Range
parseRange s =
  case map (read @Int) $ wordsOn ('-' ==) s of
    [a, b] -> Range (a, b)
    _ -> error "Invalid range"

isInvalid :: Int -> Bool
isInvalid x = even l && (take (l `div` 2) s == drop (l `div` 2) s)
  where
    s = show x
    l = length s

-- Part 2 function
isInvalid' :: Int -> Bool
isInvalid' x = any isRepeat [1 .. l `div` 2]
  where
    s = show x
    l = length s

    isRepeat k =
      l `mod` k == 0
        && concat (replicate (l `div` k) (take k s)) == s

invalidsOfRange :: Range -> [Int]
invalidsOfRange (Range (a, b))
  -- isInvalid for part1, isInvalid' for part2
  | a <= b = ([a | isInvalid a]) ++ invalidsOfRange (Range (a + 1, b))
  | otherwise = []

ranges :: IO [Range]
ranges = map parseRange . wordsOn (',' ==) <$> readFile "puzzle.txt"

sumInvalids :: IO Int
sumInvalids = sum . map (sum . invalidsOfRange) <$> ranges

main :: IO ()
main = putStrLn . ("Sum is " ++) . show =<< sumInvalids
