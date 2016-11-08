module TreeByLevels.TreeNode where

data TreeNode a = TreeNode {
      left  :: Maybe (TreeNode a),
      right :: Maybe (TreeNode a),
      value :: a
    }
                  
instance (Show a) => Show (TreeNode a) where
  show (TreeNode l r v) = "(" ++ show v ++ " " ++ show' l ++ " " ++ show' r ++ ")"
    where show' (Just n) = show n
          show' Nothing  = "_"
