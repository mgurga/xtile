module Search (search, SearchOptions(..), SearchResults(..)) where

import State
import Queue
import Lib (get_children)
import qualified Data.Set as Set
import Data.List (sortBy)

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
    seen_states :: Set.Set State,
    queue :: Queue StateResults,
    best_results :: SearchResults,
    failed :: Bool,
    done :: Bool
}

-- these parameters are returned after the search finishes
data SearchResults = SearchResults {
    depth :: Int,
    largest_queue :: Int,
    expanded_nodes :: Int,
    cost :: Int
} deriving (Show, Eq)

-- find best results given 2 search states
results_compare :: SearchResults -> SearchResults -> SearchResults
results_compare a b = do
    -- compare costs, then depths, then expaneded nodes
    if (cost a) == (cost b) then do
        if (depth a) == (depth b) then do
            if (expanded_nodes a) < (expanded_nodes b) then a else b
        else if (depth a) < (depth b) then a else b
    else if (cost a) < (cost b) then a else b

-- start search by passing options to astar_search
search :: SearchOptions -> Maybe SearchResults
search options = do
    let res = astar_search
            options
            SearchInternal {
                seen_states=(Set.empty),
                queue=
                    (Queue.singleton -- create queue with only the start node and 0 depth
                        StateResults {
                            state=(start_state options),
                            results=SearchResults {depth=0, expanded_nodes=0, largest_queue=0, cost=0}}),
                best_results=
                    SearchResults {depth=maxBound, expanded_nodes=maxBound, largest_queue=maxBound, cost=maxBound},
                done=False, failed=False
            }
    if (failed res) then
        Nothing
    else
        Just (best_results res)

-- sort states by lowest cost based on given heuristic function
sort_states :: [State] -> (State -> Int) -> [State]
sort_states given h = do
    sortBy (\s1 s2 -> (
        if (h s1) > (h s2) then LT
        else if (h s1) < (h s2) then GT
        else EQ
     )) given

-- this function recursivly calls itself with SearchInternal which holds the queue and final result
astar_search :: SearchOptions -> SearchInternal -> SearchInternal
astar_search options internal = do
    let front = Queue.dequeue (queue internal) -- get first state in queue
    let out = case front of
            Nothing -> (
                -- queue is empty return "failure"
                SearchInternal {
                    seen_states=(seen_states internal),
                    queue=(Queue.empty),
                    best_results=(best_results internal),
                    failed=True,
                    done=True
                })
            Just (val, tl) -> do
                if board (state val) == board (end_state options) || done internal then
                    -- first item in queue is goal state, mark as done and return
                    SearchInternal {
                        seen_states=(seen_states internal),
                        queue=(tl),
                        best_results=(results val),
                        done=True, failed=False
                    }
                else do
                    let children = (get_children (state val))

                    -- remove children that are same as parent e.g. invalid operator
                    -- remove children that have been seen
                    let reduced_children =
                         filter (\c -> 
                            not (Set.member c (seen_states internal)) &&
                            (board c) /= (board (state val)))
                         children

                    -- sort children by lowest cost
                    let sorted_children = 
                         sort_states reduced_children (heuristic_func options)

                    -- create sorted list of the form (State, StateResults)
                    let sorted_stats =
                         Prelude.map
                          (\sc -> (sc, SearchResults {
                              depth = (depth (results val)) + 1,
                              expanded_nodes = (expanded_nodes (results val)) + (length sorted_children),
                              largest_queue = (length tl),
                              cost = (depth (results val)) + 1 + ((heuristic_func options) sc)
                          }))
                          sorted_children

                    -- add all children nodes to queue with new results
                    let newqueue =
                         foldr (\(child, childstats) acc ->
                             (Queue.enqueue
                                 StateResults {state=child, results=childstats}
                             acc)
                         ) (tl) (sorted_stats)

                    -- add children to seen set
                    let newseen = 
                         foldr (\(child, _) acc -> 
                            Set.insert child acc
                         ) (seen_states internal) sorted_stats

                    -- call astar_search with new queue and updated best_results
                    let best_child_results =
                         case sorted_stats of
                             [] -> (results val) -- if children list is empty we've hit a dead end
                             (_, sr):_ -> sr -- otherwise return best child results

                    astar_search
                        options
                        SearchInternal {
                            seen_states=newseen,
                            queue=newqueue,
                            done=False,
                            failed=best_child_results==(results val),
                            best_results=(results_compare (best_child_results) (results val))
                        }
    out
