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
