module ExactCover where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (minimumBy)
import Control.Monad (msum)
import Data.Ord (comparing)
import Data.Function (on)


type DLX a = M.Map Int (DLXColumn a)
type DLXColumn a = S.Set (DLXRow a)
data DLXRow a = DLXRow {
    columns :: S.Set Int,
    val :: a
} deriving (Show)

instance Ord a => Ord (DLXRow a) where
    compare = comparing val

instance Eq a => Eq (DLXRow a) where
    (==) = (==) `on` val


removeRow :: Ord a => DLX a -> DLXRow a -> DLX a
removeRow dlx row = foldl (flip $ M.adjust (S.delete row)) dlx (columns row)

removeColumn :: Ord a =>  DLX a -> Int -> DLX a
removeColumn dlx idx = M.delete idx $ S.foldl removeRow dlx (M.findWithDefault S.empty idx dlx)

selectRow :: Ord a => DLX a -> DLXRow a -> DLX a
selectRow dlx row = foldl removeColumn dlx (columns row)

candidates :: Ord a => DLX a -> [DLXRow a]
candidates dlx = S.toList $ minimumBy (comparing S.size) $ M.elems dlx

solve :: (Show a, Ord a) => DLX a -> Maybe [a]
solve dlx | M.null dlx = Just []
solve dlx = msum . map (\row -> fmap (val row :) (solve (selectRow dlx row))) $ candidates dlx


solveAll :: (Show a, Ord a) => DLX a -> [[a]]
solveAll dlx | M.null dlx = [[]]
solveAll dlx = candidates dlx >>= (\row -> map (val row :) (solveAll (selectRow dlx row)))

buildFromRow :: Ord a => DLXRow a -> DLX a
buildFromRow row = M.fromList [(i, S.singleton row) | i <- S.toList (columns row)]

buildFromRows :: Ord a => [DLXRow a] -> DLX a
buildFromRows = foldl (\m row -> M.unionWith S.union m (buildFromRow row)) M.empty
