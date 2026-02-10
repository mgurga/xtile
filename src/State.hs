module State (
    State(..),
    solved_state,
    is_solved
) where

data State = State {
    size :: Int, -- side length of board
    board :: [Int], -- will be size*size length
    solved :: Bool
} deriving (Show, Eq, Ord)

-- helper function to recursivly countdown
solved_board :: Int -> Int -> [Int]
solved_board count end = do
    if count == end then
        [0] 
    else
        [count] ++ (solved_board (count + 1) end)

-- create a solved state. e.g. [8, 7, 6, 5, 4, 3, 2, 1, 0] given an Int side length
solved_state :: (Int) -> (State)
solved_state side_length =
    State {
        size = side_length,
        board = (solved_board 1 (side_length * side_length)),
        solved = True
    }

is_solved :: (State) -> (Bool)
is_solved given =
    (board given) == (board (solved_state (size given)))