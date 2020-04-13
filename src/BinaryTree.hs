module BinaryTree where

data BinaryTree a = Nil | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

singleton :: (Ord a) => a -> BinaryTree a
singleton a = Node a Nil Nil
