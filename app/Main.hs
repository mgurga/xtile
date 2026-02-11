module Main (main) where

import State (solved_state, State (..))
import Lib (shuffle_state)
import Search (search, SearchOptions(..))
import Algos
import System.Environment (getArgs)

print_help :: IO ()
print_help = do
    putStrLn "xtile - sliding tile puzzle solver"
    putStrLn "Usage:"
    putStrLn "\txtile\t\tcreate random puzzle with 100 shuffles and solve"
    putStrLn "\txtile n\t\tcreate random puzzle with n shuffles and solve"
    putStrLn "\txtile n0 n1 n2 n3 n4 n5 n6 n7 n8"
    putStrLn "\t\t\tcreates a 3x3 puzzle with n0-n8 as tiles (0 is blank tile)"

main :: IO ()
main = do
    args <- getArgs

    -- create solved 3x3 state
    let goal_state = solved_state 3

    if length args == 0 then do
        -- if no arguments passed create a hard puzzle and search
        random_state <- shuffle_state goal_state 100
        putStrLn ("random state: \n\t" ++ (show random_state))
        run_searches random_state goal_state
    else if length args == 1 && (args!!0 == "help" || args!!0 == "-h" || args!!0 == "--help") then
        print_help
    else if length args == 1 then do
        -- if one argument passed convert to Int and shuffle that many times
        random_state <- shuffle_state goal_state (read (args!!0) :: Int)
        putStrLn ("random state: " ++ (show random_state))
        run_searches random_state goal_state
    else if length args == 9 then do
        -- take arguments as 3x3 start board where 0 is blank
        let given_board = 
             [read (args!!0) :: Int, read (args!!1) :: Int, read (args!!2) :: Int,
              read (args!!3) :: Int, read (args!!4) :: Int, read (args!!5) :: Int,
              read (args!!6) :: Int, read (args!!7) :: Int, read (args!!8) :: Int]
        let given_state = State {solved=False, size=3, board=given_board}
        putStrLn ("given state: " ++ (show given_state))
        run_searches given_state goal_state
    else do
        putStrLn("invalid number of arguments")
        print_help

run_searches :: State -> State -> IO ()
run_searches start goal = do
    -- run search for manhattan distance heuristic
    let ops = SearchOptions {
        start_state=start,
        end_state=goal,
        heuristic_func=manhattan_distance_heuristic
    }
    let results = search ops
    putStrLn ("manhattan distance heuristic search: ")
    putStrLn (show results)

    -- run search for misplaced tile heuristic
    let ops1 = SearchOptions {
        start_state=start,
        end_state=goal,
        heuristic_func=misplaced_tile_heuristic
    }
    let results1 = search ops1
    putStrLn ("misplaced tile heuristic search: ")
    putStrLn ((show results1))

    -- run search for uniform cost heuristic
    let ops2 = SearchOptions {
        start_state=start,
        end_state=goal,
        heuristic_func=uniform_cost_heuristic
    }
    let results2 = search ops2
    putStrLn ("uniform cost search: ")
    putStrLn (show results2)