module Logic.Node where

type NodeIdentifier = Int

data Node a = Node {
                     nodeData :: a,
                     nodeId :: NodeIdentifier
                   } deriving (Eq, Show)