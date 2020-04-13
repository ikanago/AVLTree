module Main where

import           AVLTree

main :: IO ()
main = print $ foldr insert E [1, 4, 6, 2, 3, 8, 5]
