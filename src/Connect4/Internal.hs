module Connect4.Internal(
     is_move_on_the_board
   , rows_available
   , is_valid_move
) where

import Data.Matrix
import qualified Data.Vector

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
                                        
---------------------------------------------------------------------------------------
-- rows_available
-- DESCRIPTION: rows available are the rows with value 0 of a column of the board
-- INPUT: column board
-- OUTPUT: returns rows available in a column
-- PRE-CONDITION: column should be valid in the board. If the column is out of bounds 
--                it is going to throw an exception
-- POST-CONDITION: n/a
---------------------------------------------------------------------------------------
rows_available :: (Eq a, Num a) => Int -> Matrix a -> Int
rows_available column board = length $ Data.Vector.filter (==0) $ getCol column board

---------------------------------------------------------------------------------------
-- is_valid_move
-- DESCRIPTION: a valid move is one that is inside the limits of the board
-- INPUT: column board
-- OUTPUT: true if move is valid, false if move is invalid
-- PRE-CONDITION: n/a
-- POST-CONDITION: n/a
---------------------------------------------------------------------------------------
is_valid_move :: (Eq a, Num a) => Int -> Matrix a -> Bool
is_valid_move column board = column > 0 && 
                             column <= (ncols board) && 
                             (rows_available column board) > 0
