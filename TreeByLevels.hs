module TreeByLevels where

import TreeByLevels.TreeNode

treeByLevels :: Maybe (TreeNode a) -> [a]
treeByLevels maybeNode = bfs [] [maybeNode]
    where bfs valList []       = valList
          bfs valList nodeList = let h = head nodeList
                                     r = tail nodeList
                                 in 
                                   case h of
                                     Nothing -> bfs valList r
                                     Just n  -> bfs (valList ++ [value n]) (r ++ [left n, right n])

buildTree []     = Nothing
buildTree (v:vs) = Just $ TreeNode v (buildTree lvs) (buildTree rvs)
  where (lvs, rvs) = splitAt ((length vs + 1) `div` 2) vs

tree = buildTree [2, 8, 9, 1, 3, 4, 5]

t1 = buildTree [2, 8, 9]
t2 = buildTree [2, 8, 9, 1, 3]
