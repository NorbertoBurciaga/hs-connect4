module Connect4 (
	initiate_board
) where

import Connect4.Internal
import Data.Matrix

---------------------------------------------------------------------------------------
-- initiate_board
-- DESCRIPTION: an initial board is a matrix of rows x columns filled with 0s
-- INPUT: rows columns
-- OUTPUT: return a matrix rows x columns initialized in 0
-- PRE-CONDITION: absolute of negatives and 0 converted to 1
-- POST-CONDITION: n/a
---------------------------------------------------------------------------------------
initiate_board :: Num a => Int -> Int -> Matrix a
initiate_board rows columns 
                       | rows == 0 && columns == 0 = zero 1 1
                       | rows == 0                 = zero 1 (abs columns)
                       | columns == 0              = zero (abs rows) 1
                       | otherwise                 = zero (abs rows) (abs columns)

