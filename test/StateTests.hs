module StateTests (stateTests) where

import State (State (..), solved_state)
import Test.Tasty
import Test.Tasty.HUnit

stateTests :: TestTree
stateTests = testGroup "State tests"
  [ testCase "Solved state size 2" $
      (board (solved_state 2)) @?= [1, 2,
                                    3, 0]
    
  , testCase "Solved state size 3" $
      (board (solved_state 3)) @?= [1, 2, 3,
                                    4, 5, 6,
                                    7, 8, 0]

  , testCase "Solved state size 5" $
      (board (solved_state 5)) @?= [1,  2,  3,  4,  5,
                                    6,  7,  8,  9,  10,
                                    11, 12, 13, 14, 15,
                                    16, 17, 18, 19, 20,
                                    21, 22, 23, 24, 0]
  ]