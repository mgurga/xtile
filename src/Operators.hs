module Operators (
    operators,
    move_blank_up,
    move_blank_down,
    move_blank_left,
    move_blank_right,
    blank_index
) where

import State ( State (..), is_solved )

-- stubs for all operations that can be applied to a State

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

move_blank_up :: (State) -> (State)
move_blank_up start = do
    let bi = blank_index (board start)
    let new_bi = bi - (size start)
    if new_bi < 0 then
      start -- return input state if blank tile would move into invalid position
    else do
      let new_board = zipWith (\i element -> 
              if i == bi then 
                  (board start)!!new_bi -- if at blank tile index replace the 0 with above number
              else if i == new_bi then 
                  (board start)!!bi     -- if at above number replace the value with blank tile (0)
              else element) [0..] (board start)
      let basic_state = State { size=(size start), board=new_board, solved=False }
      State { size=(size start), board=new_board, solved=is_solved(basic_state) }

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