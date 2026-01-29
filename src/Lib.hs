module Lib (get_children) where

import State (State)
import Operators (operators)

apply_operators :: State -> [State -> State] -> [State] -> [State]
apply_operators parent (op:tl) children =
    apply_operators parent tl (children ++ [(op parent)])
apply_operators _ [] children =
    children

get_children :: (State) -> [State]
get_children given =
    (apply_operators given operators [])