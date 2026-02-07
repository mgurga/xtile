module Search (search, SearchOptions(..), SearchResults(..)) where

import State
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

-- helper data type to use with Queue
data StateResults = StateResults {
    state :: State,
    results :: SearchResults
}

-- internal search data b/w recursive search calls
data SearchInternal = SearchInternal {
    seen_states :: Set State,
    queue :: Queue StateResults,
    best_results :: SearchResults,
    done :: Bool
}

-- these parameters are returned after the search finishes
data SearchResults = SearchResults {
    depth :: Int,
    expanded_nodes :: Int,
    largest_queue :: Int
} deriving (Show, Eq)

-- find best results given 2 search states
results_compare :: SearchResults -> SearchResults -> SearchResults
results_compare a b = do
    -- compare depths, expaneded nodes, then largest_queue
    if (depth a) == (depth b) then do
        if (expanded_nodes a) == (expanded_nodes b) then do
            if (largest_queue a) < (largest_queue b) then a else b
        else if (expanded_nodes a) < (expanded_nodes b) then a else b
    else if (depth a) < (depth b) then a else b

-- start search by passing options to astar_search
search :: SearchOptions -> SearchResults
search options = do
    let res = astar_search
            options
            SearchInternal {
                seen_states=(Set.singleton (start_state options)),
                queue=
                    (Queue.singleton -- create queue with only the start node and 0 depth
                        StateResults {
                            state=(start_state options),
                            results=SearchResults {depth=0, expanded_nodes=0, largest_queue=0}}),
                best_results=
                    SearchResults {depth=maxBound, expanded_nodes=maxBound, largest_queue=maxBound},
                done=False
            }
    (best_results res)

-- this function recursivly calls itself with SearchInternal which holds the queue and final result
astar_search :: SearchOptions -> SearchInternal -> SearchInternal
astar_search options internal = do
    let front = Queue.dequeue (queue internal) -- get first state in queue
    let out = case front of
            Nothing -> (
                -- queue is empty return "failure"
                SearchInternal {
                    seen_states=(seen_states internal),
                    queue=(queue internal),
                    best_results=(best_results internal),
                    done=True
                })
            Just (val, tl) -> do
                if (state val) == (end_state options) then
                    -- first item in queue is goal state, mark as done and return
                    SearchInternal {
                        seen_states=(seen_states internal),
                        queue=(queue internal),
                        best_results=(results val),
                        done=True
                    }
                else do
                    let children = (get_children (state val))
                    -- TODO: sort children by depth + heuristic
                    let newstats = 
                         SearchResults {
                             depth = (depth (results val)) + 1,
                             expanded_nodes = (expanded_nodes (results val)) + (length children),
                             largest_queue = (length (queue internal))
                         }
                    -- add all children nodes to queue with new results
                    let newqueue =
                         foldr (\child acc -> 
                            (Queue.enqueue StateResults {state=child, results=newstats} acc)
                         ) (tl) children

                    -- call astar_search with new queue and updated best_results
                    astar_search
                        options
                        SearchInternal {
                            seen_states=(seen_states internal),
                            queue=newqueue,
                            done=False,
                            best_results=(results_compare newstats (results val))
                        }
    out
