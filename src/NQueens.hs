module NQueens (nqueens, nqueensPar) where
import ExactCover
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (minimumBy)
import Data.Ord (comparing)
import Control.Parallel.Strategies (using, parList, rseq)

candidatesRC :: Ord a => Int -> DLX a -> [DLXRow a]
candidatesRC c dlx = S.toList $ minimumBy (comparing S.size) $ map snd $ filter ((<= c) . fst) $ M.toList dlx

solveRCCount :: Ord a => Int -> DLX a -> Int
solveRCCount c dlx | M.null dlx || fst (M.findMin dlx) > c = 1
solveRCCount c dlx = sum $ map (solveRCCount c . selectRow dlx) $ candidatesRC c dlx

solveRCCountPar :: Ord a => Int -> DLX a -> Int
solveRCCountPar c dlx | M.null dlx || fst (M.findMin dlx) > c = 1
solveRCCountPar c dlx = sum (map (solveRCCount c . selectRow dlx) (candidatesRC c dlx) `using` parList rseq)

nqueens :: Int -> Int
nqueens n = solveRCCount 1500 $ buildFromRows $ [
    DLXRow (S.fromList [x, 1000 + y, 2000 + x + y, 3000 + x - y]) (x, y)
    | x <- [0 .. n - 1], y <- [0 .. n - 1]]

nqueensPar :: Int -> Int
nqueensPar n = solveRCCountPar 1500 $ buildFromRows $ [
    DLXRow (S.fromList [x, 1000 + y, 2000 + x + y, 3000 + x - y]) (x, y)
    | x <- [0 .. n - 1], y <- [0 .. n - 1]]