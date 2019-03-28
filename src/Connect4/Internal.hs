module Connect4.Internal(
   is_move_on_the_board
) where

import Data.Matrix

---------------------------------------------------------------------------------------
-- is_move_on_the_board
-- DESCRIPTION: move is inside the limits of the board
-- INPUT: row column board
-- OUTPUT: true if move is in the board, false if move is invalid
-- PRE-CONDITION: n/a
-- POST-CONDITION: n/a
---------------------------------------------------------------------------------------
is_move_on_the_board :: (Eq a, Num a) => Int -> Int -> Matrix a -> Bool
is_move_on_the_board row column board = column > 0 && 
                                        column <= (ncols board) && 
                                        row > 0 &&
                                        row <= (nrows board)
                                        
