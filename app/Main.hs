module Main where

import Sudoku
import NQueens

main :: IO ()
main = do
    let s = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
    -- print $ sudoku s
    -- print $ sudokuAll s
    -- print $ sudokuAllPar s
    -- print $ nqueens 13
    print $ nqueensPar 14