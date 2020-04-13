module BinaryTree where

data BinaryTree a = Nil | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

singleton a = Node a Nil Nil

newBinaryTree :: (Ord a) => [a] -> BinaryTree a
newBinaryTree = foldr insert Nil . reverse

insert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert x Nil = Node x Nil Nil
insert x (Node a lhs rhs) | x < a  = Node a (insert x lhs) rhs
                             | x == a = Node x lhs rhs
                             | x > a  = Node a lhs (insert x rhs)

search x Nil = False
search x (Node a lhs rhs) | x < a  = search x lhs
                             | x == a = True
                             | x > a  = search x rhs
