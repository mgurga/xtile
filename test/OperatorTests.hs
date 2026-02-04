module OperatorTests (operatorTests) where

import State (State (..), solved_state)
import Operators (move_blank_up, move_blank_down, move_blank_left, move_blank_right, blank_index)
import Test.Tasty
import Test.Tasty.HUnit

operatorTests :: TestTree
operatorTests = testGroup "Operator tests"
  [ testCase "Blank index check 1" $
      (blank_index (board (solved_state 3))) @?= 8

  , testCase "Blank index check 2" $
      (blank_index ([8, 7, 3, 6, 5, 0, 2, 1])) @?= 5

  , testCase "Blank index check 3" $
      (blank_index ([8, 7, 0, 6, 5, 3, 2, 1])) @?= 2

  , testCase "Blank index check 4" $
      (blank_index ([0, 7, 8, 6, 5, 3, 2, 1])) @?= 0

  , testCase "Move blank up test 1" $
      (board (move_blank_up (solved_state 3))) @?= [1, 2, 3,
                                                    4, 5, 0,
                                                    7, 8, 6]

    -- should not change board b/c blank tile is in top row
  , testCase "Move blank up test 2" $ do
      let test_state = State {
        solved=False, size=3, board=[
            8, 0, 6,
            5, 4, 3,
            2, 1, 7
        ]
      }
      (board (move_blank_up (test_state))) @?= 
        [8, 0, 6, 5, 4, 3, 2, 1, 7]

  , testCase "Move blank up test 3" $ do
      let test_state = State {
        solved=False, size=2, board=[
            1, 2,
            3, 0
        ]
      }
      (board (move_blank_up (test_state))) @?= 
        [1, 0, 3, 2]

  , testCase "Move blank down test 1" $
      (board (move_blank_down (solved_state 3))) @?= [1, 2, 3,
                                                      4, 5, 6,
                                                      7, 8, 0]

  , testCase "Move blank down test 2" $ do
      let test_state = State {
        solved=False, size=2, board=[
            1, 0,
            3, 2
        ]
      }
      (move_blank_down (test_state)) @?= 
        State {solved=True, size=2, board=[1, 2, 3, 0]}

  , testCase "Move blank down test 3" $ do
      let test_state = State {
        solved=False, size=3, board=[
            8, 4, 6,
            5, 0, 3,
            2, 1, 7
        ]
      }
      (board (move_blank_down (test_state))) @?= 
        [8, 4, 6, 5, 1, 3, 2, 0, 7]

  , testCase "Move blank left test 1" $
      (board (move_blank_left (solved_state 3))) @?= [1, 2, 3,
                                                      4, 5, 6,
                                                      7, 0, 8]

  , testCase "Move blank left test 2" $ do
      let test_state = State {
        solved=False, size=3, board=[
            8, 6, 4,
            0, 5, 3,
            2, 7, 1
        ]
      }
      (board (move_blank_left (test_state))) @?= 
        [8, 6, 4, 0, 5, 3, 2, 7, 1]

  , testCase "Move blank left test 3" $ do
      let test_state = State {
        solved=False, size=2, board=[
            1, 3,
            2, 0
        ]
      }
      (board (move_blank_left (test_state))) @?= 
        [1, 3, 0, 2]

  , testCase "Move blank right test 1" $
      (board (move_blank_right (solved_state 3))) @?= [1, 2, 3,
                                                       4, 5, 6,
                                                       7, 8, 0]

  , testCase "Move blank right test 2" $ do
      let test_state = State {
        solved=False, size=2, board=[
            8, 4, 6,
            5, 0, 3,
            1, 7, 2
        ]
      }
      (board (move_blank_right (test_state))) @?= 
        [8, 4, 6, 5, 3, 0, 1, 7, 2]

  , testCase "Move blank right test 3" $ do
      let test_state = State {
        solved=False, size=2, board=[
            1, 2,
            0, 3
        ]
      }
      (move_blank_right (test_state)) @?=
        State {solved=True, size=2, board=[1, 2, 3, 0]}
  ]