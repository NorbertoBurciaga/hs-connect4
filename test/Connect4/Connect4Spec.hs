module Connect4.Connect4Spec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Connect4
import Connect4.Internal
import Data.Matrix

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "initiate_board" $ do
   context "when the board is initiated with rows and columns different than 0. The absolute is used to handle negative values" $ do
     it "returns a rows x columns matrix with values initialized in 0 to represent a new game board" $ do
       property $ \rows columns -> rows /= 0 && columns /= 0 ==> 
          (abs rows) == nrows (initiate_board rows columns) && 
          (abs columns) == ncols (initiate_board rows columns)

  describe "is_move_on_the_board" $ do
    context "every move should be inside the limits of the board" $ do
      it "should validate row and column for every move to be inside the limits of the board" $ do
         is_move_on_the_board 6 4 (zero 6 7) `shouldBe` True
         is_move_on_the_board 6 8 (zero 6 7) `shouldBe` False
         is_move_on_the_board 4 4 (fromLists [[0,0,0,0],[0,0,0,0],[0,0,0,0],[1,0,2,0]]) `shouldBe` True

  describe "rows_available" $ do
    context "it is required to know how many rows are available in a column of the board" $ do
      it "should return the number of rows available in a column of the board by counting 0 values in the top of the column" $ do
         rows_available 3 (fromLists [[0,0,0,0],[0,0,0,0],[0,0,0,0],[1,0,2,0]]) `shouldBe` 3
         rows_available 1 (initiate_board 4 4) `shouldBe` 4
         rows_available 1 (fromLists [[1,0,0,0],[2,0,0,0],[1,0,0,0],[1,0,2,0]]) `shouldBe` 0
         
  describe "is_valid_move" $ do
    context "when a player plays a move, it should be inside the board and the column should have rows available" $ do
      it "should return true if the move is in the board and there is available rows in the column played" $ do
         is_valid_move 1 (initiate_board 4 4) `shouldBe` True
         is_valid_move 1 (fromLists [[1,0,0,0],[2,0,0,0],[1,0,0,0],[1,0,2,0]]) `shouldBe` False
         is_valid_move 1 (fromLists [[0,0,0,0],[2,0,0,0],[1,0,0,0],[1,0,2,0]]) `shouldBe` True

  describe "count_moves" $ do
    context "in order to win, counting moves for the same player from last move in vertical, horizontal and diagonal directions" $ do
      it "should return the number of moves for the same player from the row-column position in the direction specified" $ do
        count_moves 1 4 1 0   (fromLists [[0,0,0,0],[0,0,0,0],[0,0,0,0],[1,1,1,1]]) `shouldBe` 4
        count_moves 1 4 4 180 (fromLists [[0,0,0,0],[0,0,0,0],[0,0,0,0],[1,1,1,1]]) `shouldBe` 4
        count_moves 1 2 1 270 (fromLists [[0,0,0,0],[1,0,0,0],[1,0,0,0],[1,0,0,0]]) `shouldBe` 3
        count_moves 2 4 1 45  (fromLists [[1,0,0,2],[2,1,2,1],[1,2,1,2],[2,1,2,1]]) `shouldBe` 4
        count_moves 1 4 4 135 (fromLists [[1,0,0,2],[2,1,2,1],[1,2,1,2],[2,1,2,1]]) `shouldBe` 4
        count_moves 1 1 1 315 (fromLists [[1,0,0,2],[2,1,2,1],[1,2,1,2],[2,1,2,1]]) `shouldBe` 4
        
  describe "is_winning_position" $ do
    context "when a player makes a move, from that position verify if he wins the game by counting in all directions (except up vertical)" $ do
      it "verifies if a move wins the game" $ do
        is_winning_position 1 1 1 (fromLists [[1,0,0,2],[2,1,2,1],[1,2,1,2],[2,1,2,1]]) `shouldBe` True
        is_winning_position 2 1 4 (fromLists [[1,0,0,2],[2,1,2,1],[1,2,1,2],[2,1,2,1]]) `shouldBe` True
        