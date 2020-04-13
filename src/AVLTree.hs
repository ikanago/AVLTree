module AVLTree where

data AVLTree a = E
    | N a (AVLTree a) (AVLTree a)
    | Z a (AVLTree a) (AVLTree a)
    | P a (AVLTree a) (AVLTree a)
    deriving (Show)


singleton :: a -> AVLTree a
singleton a = Z a E E

insert :: (Ord a) => a -> AVLTree a -> AVLTree a
insert x E = Z x E E
insert x (N a left right) | x < a  = putNL a left right
                          | x > a  = N a left (insert x right)
                          | x == a = N x left right
    where putNL a E r = Z a (insert x E) r
insert x (Z a left right) | x < a  = Z a (insert x left) right
                          | x > a  = Z a left (insert x right)
                          | x == a = Z x left right
insert x (P a left right) | x < a  = P a (insert x left) right
                          | x > a  = P a left (insert x right)
                          | x == a = P x left right

height :: AVLTree a -> Int
height E         = 0
height (N _ _ r) = 1 + height r
height (Z _ l _) = 1 + height l
height (P _ l _) = 1 + height l
