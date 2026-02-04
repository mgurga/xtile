module Lib (get_children, shuffle_state) where

import State (State)
import Operators (operators)
import System.Random.Shuffle (shuffleM)

apply_operators :: State -> [State -> State] -> [State] -> [State]
apply_operators parent (op:tl) children =
    apply_operators parent tl (children ++ [(op parent)])
apply_operators _ [] children =
    children

get_children :: (State) -> [State]
get_children given =
    (apply_operators given operators [])

-- given a starting state and number n, randomly apply operations n times
-- this process ensures the state is solvable
shuffle_state :: State -> Int -> IO State
shuffle_state initial shuffles_left = do
    random_operators <- shuffleM operators
    let random_operator = random_operators!!0
    if shuffles_left == 1 then
        return (random_operator initial)
    else do
        shuffle_state (random_operator initial) (shuffles_left - 1)
