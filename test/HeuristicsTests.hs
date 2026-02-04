module HeuristicsTests (heuristicsTests) where

import State (State (..))
import Algos (misplaced_tile_heuristic, manhattan_distance_heuristic)
import Test.Tasty
import Test.Tasty.HUnit

heuristicsTests :: TestTree
heuristicsTests = testGroup "Heuristics tests"
  [ testCase "Misplaced Tile Heuristic 1" $ do
    let test_state = State {solved=False, size=2, board = [
        1, 2,
        0, 3
    ]}
    misplaced_tile_heuristic test_state @?= 1

  , testCase "Misplaced Tile Heuristic 2" $ do
    let test_state = State {solved=False, size=3, board = [
        1, 2, 3,
        4, 5, 6,
        7, 8, 0
    ]}
    misplaced_tile_heuristic test_state @?= 0

  , testCase "Misplaced Tile Heuristic 3" $ do
    let test_state = State {solved=False, size=3, board = [
        8, 1, 3,
        5, 2, 6,
        4, 7, 0
    ]}
    misplaced_tile_heuristic test_state @?= 6

  , testCase "Manhattan Distance Heuristic 1" $ do
    let test_state = State {solved=False, size=3, board = [
        1, 2, 3,
        4, 5, 6,
        7, 8, 0
    ]}
    manhattan_distance_heuristic test_state @?= 0

  , testCase "Manhattan Distance Heuristic 2" $ do
    let test_state = State {solved=False, size=2, board = [
        1, 2,
        0, 3
    ]}
    manhattan_distance_heuristic test_state @?= 1

  , testCase "Manhattan Distance Heuristic 3" $ do
    let test_state = State {solved=False, size=3, board = [
        3, 2, 8,
        4, 5, 6,
        7, 1, 0
    ]}
    manhattan_distance_heuristic test_state @?= 8

  ]