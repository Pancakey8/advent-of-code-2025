module Main (main) where
import qualified Data.Map    as M
import qualified Data.Set    as S
import           Debug.Trace (trace)
import qualified GHC.Arr     as A
import Data.Bits ((.|.))

getConns :: IO (M.Map String [String])
getConns = M.fromList . map ((\(dev, _:conns) -> (dev, words conns)) . break (==':')) . lines <$> readFile "puzzle.txt"

-- Lame, can't DFS from svr, I wanted to reuse this
pathsExits :: String -> M.Map String [String] -> [[String]]
pathsExits start conns = dfs S.empty start
  where
    dfs disc p =
      case conns M.! p of
        ["out"] -> [[ "out", p ]]
        nexts
          | p `S.member` disc -> []
          | otherwise -> concatMap (map (p :) . dfs (S.insert p disc)) nexts


part1 :: IO ()
part1 = print . length <$> pathsExits "you" =<< getConns

solve :: M.Map String [String] -> Integer
solve conns = dp A.! (svrId, 0, 0)
  where
    nodes = "out" : M.keys conns -- out is never a key
    n = length nodes

    nodeId :: M.Map String Int
    nodeId = M.fromList (zip nodes [0..])

    outId = nodeId M.! "out"
    dacId = nodeId M.! "dac"
    fftId = nodeId M.! "fft"
    svrId = nodeId M.! "svr"

    adj :: A.Array Int [Int]
    adj = A.array (0, n-1)
            [ (nodeId M.! dev, map (nodeId M.!) cs)
            | (dev, cs) <- M.toList conns ]

    -- Bounds: (node, seenDAC, seenFFT)
    bnds = ((0,0,0),(n-1,1,1))

    dp :: A.Array (Int,Int,Int) Integer
    dp = A.array bnds
            [ ((i,d,f), val i d f)
            | i <- [0..n-1], d <- [0,1], f <- [0,1] ]

    val i d f
        | i == outId = toInteger $ fromEnum $ d == 1 && f == 1
        | otherwise =
            let d' = d .|. fromEnum (i == dacId)
                f' = f .|. fromEnum (i == fftId)
            in sum [ dp A.! (j, d', f') | j <- adj A.! i ]

part2 :: IO ()
part2 = print <$> solve =<< getConns

main :: IO ()
main = part2
