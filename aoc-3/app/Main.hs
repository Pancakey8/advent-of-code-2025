module Main (main) where
import Data.Maybe (fromMaybe)

largestNumber :: String -> String
largestNumber s = [d1, d2]
  where d1 = maximum $ init s
        s' = tail $ dropWhile (/= d1) s
        d2 = maximum s'

largestN :: Int -> String -> Maybe String
largestN 0 _ = Just ""
largestN n s
  | length s < n = Nothing
  | otherwise =
      let
        candidates = take (length s - n + 1) s
        d = maximum candidates
        rest = drop 1 $ dropWhile (/= d) s
      in
        fmap (d:) (largestN (n-1) rest)

numbers :: IO [String]
numbers = lines <$> readFile "puzzle.txt"

largests :: IO [String]
-- largests = map (fromMaybe "0" . largestN 12) <$> numbers
largests = map largestNumber <$> numbers

answer :: IO Int
answer = sum . map read <$> largests

main :: IO ()
main = print =<< answer
