data BinaryTree = Nil | Node BinaryTree Int BinaryTree deriving(Eq, Show)

emptyTree = Nil

containsElement Nil _ = False
containsElement (Node lt v rt) x
  | x == v = True
  | x < v = containsElement lt x
  | x > v = containsElement rt x

insert Nil x = Node Nil x Nil
insert tree@(Node lt v rt) x
  | x < v = Node (insert lt x) v rt
  | x > v = Node lt v (insert rt x)
  | x == v = tree

remove Nil _ = Nil
remove tree@(Node lt v rt) x
  | x < v = Node (remove lt x) v rt
  | x > v = Node lt v (remove rt x)
  | x == v = replace tree
  where
    replace (Node lt _ Nil) = lt
    replace (Node lt _ rt) =
      let rmin = findmin rt
          findmin (Node Nil v _) = v
          findmin (Node lt _ _) = findmin lt
      in Node lt rmin (remove rt rmin)

nearestGE tree x = helper tree 0
    where helper Nil best = best
          helper (Node lt v rt) best
            | x < v  = helper lt v
            | x > v = helper rt best
            | otherwise = x

leftTree = Node (Node Nil 1 Nil) 2 (Node Nil 3 Nil)
rightTree = Node (Node Nil 5 Nil) 6 (Node Nil 7 Nil)
bigTree = Node leftTree 4 rightTree

treeFromList [] = emptyTree
treeFromList (x:xs) = (treeFromList xs) `insert` x

listFromTree Nil = []
listFromTree (Node lt v rt) =  (listFromTree lt) ++ [v] ++ (listFromTree rt)
