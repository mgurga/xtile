module Search (SearchResults) where

data SearchResults = SearchResults {
    depth :: Int,
    expanded_nodes :: Int,
    largest_queue :: Int
}

