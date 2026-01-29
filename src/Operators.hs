module Operators (operators) where

import State

-- stubs for all operations that can be applied to a State

move_blank_up :: (State) -> (State)
move_blank_up start =
    start

move_blank_down :: (State) -> (State)
move_blank_down start =
    start

move_blank_left :: (State) -> (State)
move_blank_left start =
    start

move_blank_right :: (State) -> (State)
move_blank_right start =
    start

operators :: [State -> State]
operators = [move_blank_up, move_blank_down, move_blank_left, move_blank_right]