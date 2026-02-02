module StateTree(StateTree, state_tree_from_state) where

import State (State)
import Lib (get_children)

data RoseTree s = Child s | Parent s [RoseTree s]
type StateTree = RoseTree State

state_tree_from_state :: (State) -> (StateTree)
state_tree_from_state s =
    Parent s (map (\c -> Child c) (get_children s))