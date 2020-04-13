module Main where

import           BinaryTree

main :: IO ()
main = do
    print tree
    print $ search 5 tree
    print $ search 11 tree
    print $ flatten tree
    where tree = delete 3 $ newBinaryTree [5, 3, 7, 2, 4, 1, 9, 10, 6]