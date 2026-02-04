module Main (main) where

import State (solved_state)
import Lib (get_children, shuffle_state)
import Data.Foldable (for_)

main :: IO ()
main = do
    -- generate random State that is solvable
    let goal_state = solved_state 3
    random_state <- shuffle_state goal_state 20

    -- print state and all its children
    putStrLn ("random state: " ++ (show random_state))
    putStrLn ("children: ")
    let childarr = map (\child -> ("\t" ++ (show child))) (get_children random_state)
    for_ childarr putStrLn