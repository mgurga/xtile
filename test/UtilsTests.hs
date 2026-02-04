module UtilsTests (utilsTests) where

import State (State (..))
import Utils (sum_list, item_index, target_row_col)
import Test.Tasty
import Test.Tasty.HUnit

utilsTests :: TestTree
utilsTests = testGroup "Utils tests"
  [ testCase "Sum List" $
      (sum_list [1, 5, 7, 9]) @?= 22
    
  , testCase "Item Index" $
      (item_index [1, 1, 1, 5, 2, 1, 1] 5) @?= 3

  , testCase "Target Row Col 1" $ do
    let test_state = State { solved=False, size=3, board=[
        1, 5, 4,
        3, 0, 2,
        6, 8, 7
    ]}
    (target_row_col test_state 1) @?= (0, 0)

  , testCase "Target Row Col 2" $ do
    let test_state = State { solved=False, size=3, board=[
        1, 5, 4,
        3, 0, 2,
        6, 8, 7
    ]}
    (target_row_col test_state 7) @?= (2, 2)

  , testCase "Target Row Col 3" $ do
    let test_state = State { solved=False, size=3, board=[
        1, 5, 4,
        3, 0, 2,
        6, 8, 7
    ]}
    (target_row_col test_state 6) @?= (2, 0)
  ]