module Utils (sum_list, item_index, target_row_col) where

import State ( State(size, board) )

sum_list :: [Int] -> Int
sum_list [] = 0
sum_list (x:tl) = x + sum_list tl

item_index :: [Int] -> Int -> Int
item_index arr item = do
    let mask = zipWith (\arritem countitem -> if arritem == item then countitem else 0) arr [0..]
    sum_list mask

target_row_col :: State -> Int -> (Int, Int)
target_row_col given target = do
    let given_index = item_index (board given) target
    (given_index `div` (size given), given_index `mod` (size given))