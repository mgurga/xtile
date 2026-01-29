import State (State (board), solved_state)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [stateTests]

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
