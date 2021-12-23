module NQueens (nqueens, nqueensPar) where
import ExactCover
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (minimumBy)
import Data.Ord (comparing)
import Control.Parallel.Strategies (parMap, rseq)

candidatesRC :: Ord a => Int -> DLX a -> [DLXRow a]
candidatesRC c dlx = S.toList $ minimumBy (comparing S.size) $ map snd $ filter ((<= c) . fst) $ M.toList dlx

solveRCCount :: Ord a => Int -> DLX a -> Int
solveRCCount c dlx | M.null dlx || fst (M.findMin dlx) > c = 1
solveRCCount c dlx = sum $ map (solveRCCount c . selectRow dlx) $ candidatesRC c dlx

solveRCCountPar :: Ord a => Int -> Int -> DLX a -> Int
solveRCCountPar _ c dlx | M.null dlx || fst (M.findMin dlx) > c = 1
solveRCCountPar level c dlx = 
    let solver = if level == 0 then solveRCCount else solveRCCountPar (level - 1) in
        sum $ parMap rseq (solver c . selectRow dlx) (candidatesRC c dlx)

nqueens :: Int -> Int
nqueens n = solveRCCount 1500 $ buildFromRows $ [
    DLXRow (S.fromList [x, 1000 + y, 2000 + x + y, 3000 + x - y]) (x, y)
    | x <- [0 .. n - 1], y <- [0 .. n - 1]]

nqueensPar :: Int -> Int -> Int
nqueensPar level n = solveRCCountPar level 1500 $ buildFromRows $ [
    DLXRow (S.fromList [x, 1000 + y, 2000 + x + y, 3000 + x - y]) (x, y)
    | x <- [0 .. n - 1], y <- [0 .. n - 1]]