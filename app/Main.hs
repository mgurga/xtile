module Main (main) where

import System.Random.Shuffle (shuffleM)
import State (State(..), solved_state)
import Lib (get_children)
import Data.Foldable (for_)

main :: IO ()
main = do
    -- generate random State (might not be solvable)
    let goal_state = solved_state 3
    random_board <- shuffleM (board goal_state)
    let random_state = State {size=3, board=random_board, solved=False}

    -- print state and all its children
    putStrLn ("random state: " ++ (show random_state))
    putStrLn ("children: ")
    let childarr = map (\child -> ("\t" ++ (show child))) (get_children random_state)
    for_ childarr putStrLn