import State (State (..), solved_state)
import Operators (move_blank_up, move_blank_down, move_blank_left, move_blank_right, blank_index)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [stateTests, operatorTests]

stateTests :: TestTree
stateTests = testGroup "State tests"
  [ testCase "Solved state size 2" $
      (board (solved_state 2)) @?= [3, 2,
                                    1, 0]
    
  , testCase "Solved state size 3" $
      (board (solved_state 3)) @?= [8, 7, 6,
                                    5, 4, 3,
                                    2, 1, 0]

  , testCase "Solved state size 5" $
      (board (solved_state 5)) @?= [24, 23, 22, 21, 20,
                                    19, 18, 17, 16, 15,
                                    14, 13, 12, 11, 10,
                                    9,  8,  7,  6,  5,
                                    4,  3,  2,  1,  0]
  ]

operatorTests :: TestTree
operatorTests = testGroup "Operator tests"
  [ testCase "Blank index check 1" $
      (blank_index (board (solved_state 3))) @?= 8

  , testCase "Blank index check 2" $
      (blank_index ([8, 7, 3, 6, 5, 0, 2, 1])) @?= 5

  , testCase "Blank index check 3" $
      (blank_index ([8, 7, 0, 6, 5, 3, 2, 1])) @?= 2

  , testCase "Move blank up test 1" $
      (board (move_blank_up (solved_state 3))) @?= [8, 7, 6,
                                                    5, 4, 0,
                                                    2, 1, 3]

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
  ]
