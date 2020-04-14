module BinaryTree where

data (Ord a, Eq a) => BinaryTree a = Nil 
    | Node a (BinaryTree a) (BinaryTree a) 
    deriving (Show)

singleton a = Node a Nil Nil

-- Construct BinaryTree from List. --
newBinaryTree :: (Ord a, Eq a) => [a] -> BinaryTree a
newBinaryTree = foldr insert Nil . reverse

-- Insert a new element while keeping BinaryTree condition. --
insert :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
insert x Nil = Node x Nil Nil
insert x (Node a lhs rhs) | x == a = Node a lhs rhs
                          | x < a  = Node a (insert x lhs) rhs
                          | x > a  = Node a lhs (insert x rhs)

-- Check if a certain value is contained. --
search :: (Ord a, Eq a) => a -> BinaryTree a -> Bool
search x Nil = False
search x (Node a lhs rhs) | x == a = True
                          | x < a  = search x lhs
                          | x > a  = search x rhs

-- Get a minimum value in the tree. --
searchMin :: (Ord a, Eq a) => BinaryTree a -> a
searchMin (Node a Nil _) = a
searchMin (Node _ lhs _) = searchMin lhs

-- Delete a certain element in the tree. --
delete :: (Ord a, Eq a) => a -> BinaryTree a -> BinaryTree a
delete x (Node a lhs rhs) 
    | x < a = Node a (delete x lhs) rhs
    | x > a = Node a lhs (delete x rhs)
    | x == a = delete' (Node a lhs rhs)
    where delete' (Node a Nil rhs) = rhs
          delete' (Node a lhs Nil) = lhs
          delete' (Node a lhs rhs) = Node (searchMin rhs) lhs (delete' rhs)

-- Go through the tree in order and append a value in a visited node to a list. --
flatten :: (Ord a, Eq a) => BinaryTree a -> [a]
flatten Nil = []
flatten (Node a lhs rhs) = flatten lhs ++ [a] ++ flatten rhs
