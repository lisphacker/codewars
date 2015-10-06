module TreeByLevels.TreeNode where

data TreeNode a = TreeNode {
      value :: a,
      left  :: Maybe (TreeNode a),
      right :: Maybe (TreeNode a)
    } deriving Show

