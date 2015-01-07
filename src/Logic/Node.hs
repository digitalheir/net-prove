module Logic.Node where

type NodeIdentifier = Int

data Node a = Node {
                     nodeData :: a,
                     nodeId :: NodeIdentifier
                   } | NilNode deriving (Eq)

instance (Show a) => Show (Node a) where
  show n = (show (nodeData n))++"^^"++(show (nodeId n))