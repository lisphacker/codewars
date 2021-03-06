module TreeByLevels where

import TreeByLevels.TreeNode

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels maybeNode = bfs [] [maybeNode] []
  where bfs valList []           []           = reverse valList
        bfs valList []           nextNodeList = bfs valList (reverse nextNodeList) []
        bfs valList (n:ns)       nextNodeList = case n of
          Nothing -> bfs valList ns nextNodeList
          Just n  -> bfs ((value n):valList) ns ((right n):(left n):nextNodeList)
            
treeByLevels' :: Maybe (TreeNode a) -> [a]
treeByLevels' maybeNode = bfs [] [maybeNode]
    where bfs valList []       = valList
          bfs valList nodeList = let h = head nodeList
                                     r = tail nodeList
                                 in 
                                   case h of
                                     Nothing -> bfs valList r
                                     Just n  -> bfs (valList ++ [value n]) (r ++ [left n, right n])


----------------------------------------


buildTree :: [a] -> Maybe (TreeNode a)
buildTree []     = Nothing
buildTree (v:vs) = Just $ TreeNode (buildTree lvs) (buildTree rvs) v
  where (lvs, rvs) = splitAt ((length vs + 1) `div` 2) vs

maxDepth :: Maybe (TreeNode a) -> Int
maxDepth Nothing = 0
maxDepth (Just (TreeNode l r _)) = 1 + (max (maxDepth l) (maxDepth r))


tree = buildTree [2, 8, 9, 1, 3, 4, 5]

t1 = buildTree [2, 8, 9]
t2 = buildTree [2, 8, 9, 1, 3]
t3 = buildTree [2, 8, 9, 1, 3, 4, 5]
