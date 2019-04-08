module Connect4.Internal(
     is_move_on_the_board
   , rows_available
   , is_valid_move
   , count_moves
   , is_winning_position
   , next_possible_moves
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

---------------------------------------------------------------------------------------
-- count_moves
-- DESCRIPTION: in order to win, counting moves for the same player from last move in  
--              vertical, horizontal an diagonal directions
-- INPUT: player row column direction board
-- OUTPUT: number of moves for a player from the row-column position in the direction specified 
-- PRE-CONDITION: n/a
-- POST-CONDITION: n/a
---------------------------------------------------------------------------------------
count_moves :: (Num a, Eq a) => a -> Int -> Int -> Int -> Matrix a -> Int
count_moves player row column direction board =
   if is_move_on_the_board row column board &&
      board ! (row, column) == player
   then
      case direction of
         0   -> 1 + count_moves player  row      (column + 1) direction board
         45  -> 1 + count_moves player (row - 1) (column + 1) direction board
         135 -> 1 + count_moves player (row - 1) (column - 1) direction board
         180 -> 1 + count_moves player  row      (column - 1) direction board
         225 -> 1 + count_moves player (row + 1) (column - 1) direction board
         270 -> 1 + count_moves player (row + 1)  column      direction board
         315 -> 1 + count_moves player (row + 1) (column + 1) direction board
         otherwise -> 0
   else 0

---------------------------------------------------------------------------------------
-- is_winning_position
-- DESCRIPTION: verifies if a move wins the game
-- INPUT: player row column board
-- OUTPUT: true if player wins, false if player doesn't win
-- PRE-CONDITION: row-column position should be a valid move
-- POST-CONDITION: n/a
---------------------------------------------------------------------------------------
is_winning_position :: (Num a, Eq a) => a -> Int -> Int -> Matrix a -> Bool
is_winning_position player row column board = 
   ((count_moves player  row (column - 1) 180 board + count_moves player row (column + 1) 0 board) >=3) ||
   ((count_moves player (row - 1) (column + 1) 45 board + count_moves player (row + 1) (column - 1) 225 board) >=3) ||
   (count_moves player (row + 1) column 270 board >=3) ||
   ((count_moves player (row - 1) (column - 1) 135 board + count_moves player (row + 1) (column + 1) 315 board) >=3)

---------------------------------------------------------------------------------------
-- next_possible_moves
-- DESCRIPTION: At any point of the game a player wants to know what moves can perform next
-- INPUT: board
-- OUTPUT: list of possible next moves
-- PRE-CONDITION: 
-- POST-CONDITION: n/a
---------------------------------------------------------------------------------------
next_possible_moves :: (Eq a, Num a) => Matrix a -> [Int]
next_possible_moves board = [ x | x <-[1..(ncols board)], is_valid_move x board]


