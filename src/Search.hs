module Search (search, SearchOptions(..), SearchResults(..)) where

import State
import StateTree (StateTree, state_tree_from_state)
import Queue
import Data.Set (Set)
import Lib (get_children)
import qualified Data.Set as Set

-- these settings are passed to the search function to start the search
data SearchOptions = SearchOptions {
    start_state :: State,
    end_state :: State,
    heuristic_func :: (State -> Int)
}

-- internal search data b/w recursive search calls
data SearchInternal = SearchInternal {
    tree :: StateTree,
    seen_states :: Set State,
    queue :: Queue State,
    interim_results :: SearchResults
}

-- these parameters are returned after the search finishes
data SearchResults = SearchResults {
    depth :: Int,
    expanded_nodes :: Int,
    largest_queue :: Int
} deriving (Show, Eq)

search :: SearchOptions -> SearchResults
search options = do
    let res = astar_search
            options
            SearchInternal {
                tree=state_tree_from_state (start_state options),
                seen_states=(Set.singleton (start_state options)),
                queue=(Queue.fromList (get_children (start_state options))),
                interim_results=SearchResults {depth=0, expanded_nodes=0, largest_queue=0}
            }
    (interim_results res)

astar_search :: SearchOptions -> SearchInternal -> SearchInternal
astar_search options internal = do
    SearchInternal {
        tree=(tree internal),
        seen_states=(seen_states internal),
        queue=(queue internal),
        interim_results=(interim_results internal)
    }

