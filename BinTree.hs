module BinTree (
  BinTree,
  nil,
  is_nil,
  is_node,
  left_sub,
  left_most,
  right_sub,
  right_most,
  node_val,
  search,
  insert,
  delete
) where

data BinTree a = Nil | Node a (BinTree a) (BinTree a)

nil :: BinTree a
nil = Nil

is_nil :: BinTree a -> Bool
is_nil Nil = True
is_nil _ = False

is_node :: BinTree a -> Bool
is_node Nil = False
is_node _ = True

left_sub :: BinTree a -> BinTree a
left_sub Nil = error "empty left"
left_sub (Node _ ls _) = ls

left_most :: BinTree a -> BinTree a
left_most t
  | is_nil t = error "empty tree"
  | is_nil ls = t
  | otherwise = left_most ls
    where ls = left_sub t

right_sub :: BinTree a -> BinTree a
right_sub Nil = error "empty right"
right_sub (Node _ _ rs) = rs

right_most :: BinTree a -> BinTree a
right_most t
  | is_nil t = error "empty tree"
  | is_nil rs = t
  | otherwise = right_most rs
    where rs = right_sub t

node_val :: BinTree a -> a
node_val Nil = error "empty node"
node_val (Node n _ _) = n

search :: Ord a => a -> BinTree a -> Bool
search _ Nil = False
search x (Node n ls rs)
  | x < n = search x ls
  | x > n = search x rs
  | otherwise = True

insert :: Ord a => a -> BinTree a -> BinTree a
insert new Nil = Node new Nil Nil
insert new (Node n ls rs)
  | new > n = Node n ls (insert new rs)
  | new <= n = Node n (insert new ls) rs

delete :: Ord a => a -> BinTree a -> BinTree a
delete _ Nil = Nil
delete val (Node n ls rs)
-- searching for the one
  | val > n = Node n ls (delete val rs)
  | val < n = Node n (delete val ls) rs
-- start to delete, 3 conditions:
  | is_nil rs = ls
  | is_nil (left_sub rs) = Node (node_val rs) ls (right_sub rs)
  | otherwise = 
      let leftmost = (node_val . left_most) rs
      in Node leftmost ls (delete leftmost rs)

depth :: BinTree a -> Int -> Int
depth Nil n = n
depth (Node _ ls rs) n = max (depth ls (n + 1)) (depth rs (n + 1))

-- traverse
preorder :: BinTree a -> [a]
preorder t = preorder' t []
  where preorder' Nil acc = acc
        preorder' (Node n ls rs) acc = n : (preorder' ls (preorder' rs acc))

inorder :: BinTree a -> [a]
inorder t = inorder' t []
  where inorder' Nil acc = acc
        inorder' (Node n ls rs) acc = inorder' ls (n : (inorder' rs acc))

postorder :: BinTree a -> [a]
postorder t = postorder' t []
  where postorder' Nil acc = acc
        postorder' (Node n ls rs) acc = postorder' ls (postorder' rs acc) ++ [n]
