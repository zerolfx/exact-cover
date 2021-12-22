module Sudoku (sudoku, sudokuAll, sudokuAllPar) where

import ExactCover
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char (ord, chr)
import Control.Parallel.Strategies (using, parList, rseq)

choices :: String -> [(Int, Int, Int)]
choices s =
    [(x, y, ord c - ord '1') | (i, c) <- zip [0..] s, c /= '.', let (x, y) = (i `div` 9, i `mod` 9)] ++
    [(x, y, v) | (i, c) <- zip [0..] s, c == '.', let (x, y) = (i `div` 9, i `mod` 9), v <- [0..8]]

areaPos :: (Int, Int) -> Int
areaPos (x, y) = 3 * (x `div` 3) + (y `div` 3)

genRow :: (Int, Int, Int) -> DLXRow (Int, Int, Int)
genRow (x, y, v) = DLXRow (S.fromList [x * 9 + v, 1000 + y * 9 + v, 2000 + x * 9 + y, 3000 + areaPos (x, y) * 9 + v]) (x, y, v)

genSolution :: [(Int, Int, Int)] -> String
genSolution pos = let m = M.fromList [((x, y), v) | (x, y, v) <- pos] in
    map (\i -> chr (ord '1' + m M.! (i `div` 9, i `mod` 9))) [0..81 - 1]


sudoku :: String -> Maybe String
sudoku s = genSolution <$> solve (buildFromRows (map genRow (choices s)))

sudokuAll :: String -> [String]
sudokuAll s = genSolution <$> solveAll (buildFromRows (map genRow (choices s)))


solveAllPar :: Ord a => DLX a -> [[a]]
solveAllPar dlx | M.null dlx = [[]]
solveAllPar dlx = candidates dlx >>= (\row -> map (val row :) (solveAllPar (selectRow dlx row)) `using` parList rseq)

sudokuAllPar :: String -> [String]
sudokuAllPar s = genSolution <$> solveAllPar (buildFromRows (map genRow (choices s)))