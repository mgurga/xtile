module Operators (
    operators,
    move_blank_up,
    move_blank_down,
    move_blank_left,
    move_blank_right,
    blank_index
) where

import State ( State (..), is_solved )

-- helper to recursivly find index of 0 in puzzle board (Int list)
blank_index_rec :: [Int] -> Int -> Int
blank_index_rec [] acc = acc
blank_index_rec (x:tl) acc =
    if x /= 0 then
        blank_index_rec tl (acc+1)
    else
        acc

blank_index :: [Int] -> Int
blank_index b = blank_index_rec b 0

-- switch board tiles given a state, first tile index, and second tile index
switch_tile :: State -> Int -> Int -> State
switch_tile given fsti sndi = do
    let new_board = zipWith (\i element ->
            if i == fsti then
                (board given)!!sndi -- if at blank tile index replace the 0 with target number
            else if i == sndi then 
                (board given)!!fsti -- if at target number replace the value with blank tile (0)
            else element)
            [0..]
            (board given)
    let basic_state = State { size=(size given), board=new_board, solved=False }
    State { size=(size given), board=new_board, solved=is_solved(basic_state) }

move_blank_up :: (State) -> (State)
move_blank_up start = do
    let bi = blank_index (board start)
    let new_bi = bi - (size start)
    if new_bi < 0 then
      start -- return input state if blank tile would move into invalid position
    else
      switch_tile start bi new_bi

move_blank_down :: (State) -> (State)
move_blank_down start = do
    let bi = blank_index (board start)
    let new_bi = bi + (size start)
    if new_bi > (size start * size start) - 1 then
      start
    else
      switch_tile start bi new_bi

move_blank_left :: (State) -> (State)
move_blank_left start = do
    let bi = blank_index (board start)
    let new_bi = bi - 1
    if (bi `mod` (size start)) == 0 then
      start -- return input state if blank tile is on left border
    else
      switch_tile start bi new_bi

move_blank_right :: (State) -> (State)
move_blank_right start = do
    let bi = blank_index (board start)
    let new_bi = bi + 1
    if ((bi + 1) `mod` (size start)) == 0 then
      start
    else
      switch_tile start bi new_bi

operators :: [State -> State]
operators = [move_blank_left, move_blank_down, move_blank_right, move_blank_up]