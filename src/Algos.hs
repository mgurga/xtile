module Algos (misplaced_tile_heuristic, manhattan_distance_heuristic) where

import State (State(..), solved_state)
import Utils (target_row_col, sum_list)

misplaced_tile_heuristic :: State -> Int
misplaced_tile_heuristic given = do
    let solve = solved_state (size given)
    -- iterates through both the solved board and given state board then creates a bool list where:
    --  True means given state tile is equal to the solved state tile or solved state is blank tile
    --  False means given state tile differs from the solved state tile
    let mask = zipWith 
            (\state_tile solve_tile -> 
                (if state_tile == solve_tile || solve_tile == 0 then True else False))
            (board given)
            (board solve)
    length (filter (\v -> v == False) mask) -- return number of Falses in mask boolean list

-- helper to find distance b/w a tile and its goal position
-- arguments are given state, goal state, and target tile. returns distance
man_tile_dist :: State -> State -> Int -> Int
man_tile_dist given goal target_val = do
    let (ix, iy) = target_row_col given target_val -- row and col of given state's target val
    let (ox, oy) = target_row_col goal target_val  -- row and col of goal state's target val
    abs(ox - ix) + abs(oy - iy)

manhattan_distance_heuristic :: State -> Int
manhattan_distance_heuristic given = do
    let solve = solved_state (size given)
    let mask = zipWith 
            (\state_tile solve_tile ->
                (if state_tile == solve_tile || solve_tile == 0 then
                    0
                    else
                        man_tile_dist given solve state_tile))
            (board given)
            (board solve)
    sum_list mask