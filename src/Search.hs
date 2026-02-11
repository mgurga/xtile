module Search (search, SearchOptions(..), SearchResults(..)) where

import State
import Lib (get_children)
import qualified Data.Set as Set
import qualified Data.PQueue.Prio.Min as MinPQueue
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
    queue :: MinPQueue.MinPQueue Int StateResults,
    best_results :: SearchResults,
    interim_expanded :: Int,
    failed :: Bool,
    done :: Bool
}

-- these parameters are returned after the search finishes
data SearchResults = SearchResults {
    depth :: Int,
    largest_queue :: Int,
    expanded_nodes :: Int
} deriving (Show, Eq)

-- start search by passing options to astar_search
search :: SearchOptions -> Maybe SearchResults
search options = do
    let res = astar_search
            options
            SearchInternal {
                seen_states=(Set.empty),
                queue=
                    (MinPQueue.singleton -- create queue with only the start node and 0 depth
                        ((heuristic_func options) (start_state options))
                        StateResults {
                            state=(start_state options),
                            results=SearchResults {depth=0, expanded_nodes=0, largest_queue=0}}),
                best_results=
                    SearchResults {depth=maxBound, expanded_nodes=maxBound, largest_queue=maxBound},
                interim_expanded=0, done=False, failed=False
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
    let front = MinPQueue.minViewWithKey (queue internal) -- get first state in queue
    let out = case front of
            Nothing -> (
                -- queue is empty return "failure"
                SearchInternal {
                    seen_states=(seen_states internal),
                    queue=(MinPQueue.empty),
                    best_results=(best_results internal),
                    failed=True,
                    done=True,
                    interim_expanded=(interim_expanded internal)
                })
            Just ((_, val), tl) -> do
                if board (state val) == board (end_state options) || done internal then
                    -- first item in queue is goal state, mark as done and return
                    SearchInternal {
                        seen_states=(seen_states internal),
                        queue=tl,
                        best_results=(results val),
                        interim_expanded=(interim_expanded internal), done=True, failed=False
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
                              expanded_nodes = (interim_expanded internal),
                              largest_queue = (length tl)
                          }))
                          sorted_children

                    -- add all children nodes to queue with new results
                    let newqueue =
                         foldr (\(child, childstats) acc ->
                             (MinPQueue.insert
                                 ((depth childstats) + ((heuristic_func options) child))
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
                            interim_expanded=(interim_expanded internal) + 1,
                            best_results=(best_child_results)
                        }
    out
