module Main (main) where
import           Control.Monad                   (msum)
import           Data.Bits                       (setBit, shiftR, xor, (.&.))
import qualified Data.Map.Lazy                   as Map
import           Data.Maybe                      (fromJust, listToMaybe,
                                                  mapMaybe)
import           Data.Scientific                 (Scientific, fromFloatDigits,
                                                  toRealFloat)
import qualified Data.Set                        as S
import           Debug.Trace                     (trace)
import           GHC.Float                       (int2Double)
import           Numeric.Optimization.MIP        ((.==.), (.>=.))
import qualified Numeric.Optimization.MIP        as MIP
import           Numeric.Optimization.MIP.Solver as MS

type Lights = Integer

parseLights :: String -> (Lights, String)
parseLights [] = error "parseLights: Failed"
parseLights (_:xs) =
  case break (==']') xs of
    (lights, _:rest) -> (foldl (\acc (i, c) -> if c == '#'
                                               then setBit acc i
                                               else acc) 0 $ zip [0..] lights, tail rest)
    _ -> error "parseLights: Failed"

type Button = Integer

parseTuple :: String -> [Int]
parseTuple (_:xs) = read $ "[" ++ (init xs) ++ "]"

parseButtons :: String -> ([Button], String)
parseButtons input = (map (foldl setBit 0 . parseTuple) btns, last ws)
  where
    ws = words input
    btns = init ws

type Joltage = [Int]

parseJoltage :: String -> Joltage
parseJoltage = parseTuple

parseMachine :: String -> (Lights, [Button], Joltage)
parseMachine input = (lights, buttons, joltage)
  where
    (lights, input') = parseLights input
    (buttons, input'') = parseButtons input'
    joltage = parseJoltage input''

chooseN :: [a] -> Int -> [[a]]
chooseN _ 0 = [[]]
chooseN [] _ = []
chooseN (x:xs) k =
  map (x:) (xs `chooseN` (k - 1)) ++ (xs `chooseN` k)

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust f = listToMaybe . mapMaybe f

solveLights :: Lights -> [Button] -> Maybe Int
solveLights goal btns = length <$> firstJust tryK [0..length btns - 1]
  where
    tryK k =
      let inds = [0..length btns - 1]
          cs = inds `chooseN` k
      in firstJust (\is -> if foldl xor 0 [ btns !! i | i <- is ] == goal
                           then Just is
                           else Nothing) cs

-- Bruteforce
part1 :: IO ()
part1 = do
  machines <- map parseMachine . lines <$> readFile "puzzle.txt"
  print $ sum $ map (\(goal, btns, _) -> fromJust $ solveLights goal btns) machines

-- Should've parsed as list and implemented [Int] -> Integer
-- Oh well...
expandBitmap :: Integer -> [Int]
expandBitmap bm = go bm 0 []
  where
    go 0 _ acc = reverse acc
    go x i acc =
      go (x `shiftR` 1)
         (i + 1)
         (if (x .&. 1) == 1 then i : acc else acc)

type JoltButton = [Int]

solveJoltage :: [JoltButton] -> Joltage -> IO Double
solveJoltage btns target = do
  let varNames = map (MIP.toVar . ("x"++) . show) [0..length btns - 1]
      vars = map MIP.varExpr varNames
      targetSystem = zipWith constructTarget target [0..length target - 1]
      constructTarget targ i = MIP.constExpr (fromFloatDigits $ int2Double targ) .==. sum (map snd $ filter (\(btn, _) -> i `elem` btn) $ zip btns vars)
      problem = MIP.def {
        MIP.objectiveFunction = MIP.def { MIP.objDir = MIP.OptMin, MIP.objExpr = sum vars },
        MIP.constraints = targetSystem,
        MIP.varDomains = Map.fromList $ zip [MIP.toVar $ "x" ++ show i | i <- [0..length btns - 1]] $ repeat (MIP.IntegerVariable, (0, MIP.PosInf))
        }
  sol <- MS.solve MS.cbc MIP.def { MS.solveTimeLimit = Nothing } problem
  -- print $ MIP.solStatus sol
  -- print sol
  return $ toRealFloat $ fromJust $ MIP.solObjectiveValue sol

part2 :: IO ()
part2 = do
  machines <- map parseMachine . lines <$> readFile "puzzle.txt"
  sols <- mapM (\(_, btns, joltage) -> solveJoltage (map expandBitmap btns) joltage) machines
  print $ sum sols

main :: IO ()
main = part1
