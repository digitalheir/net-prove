module Main where
import LG.Graph
import LG.Unfold
import LG.Identify
import Data.IORef
import qualified Data.Map as Map
   --     getLinkInfo s (a :<×>: b) = [a :<×>: b]    :●: [a, b] -- L⊗
     --   getLinkInfo s (a  :\:  b) = [a, a :\: b]   :○: [b]    -- L\
      --  getLinkInfo s (a :</>: b) = [a :</>: b, b] :●: [a] -- L⊘
       -- getLinkInfo s (a :<+>: b) = [a :<+> b]     :○: [a, b] -- L⊕
        --getLinkInfo s (b :<\>: a) = [b, b :<\>: a] :●: [a] -- L⃠


insertNodes :: CompositionGraph -> [(Identifier, NodeInfo)] -> CompositionGraph
insertNodes graph [] = graph
insertNodes graph ((id,formula):ns) = insertNodes (Map.insert id formula graph) ns

addFormula :: CompositionGraph -> NodeInfo -> (CompositionGraph, Identifier)
addFormula g f = (Map.insert nextId f g,nextId)
  where nextId = Map.size g

addFormulas :: CompositionGraph -> [NodeInfo] -> (CompositionGraph, [Identifier])
addFormulas g f = addFormulas' g f []
  where addFormulas' :: CompositionGraph -> [NodeInfo] -> [Identifier] -> (CompositionGraph, [Identifier])
        addFormulas' gr []     acc = (gr,reverse acc)
        addFormulas' gr (f:fs) acc = addFormulas' g' fs (id:acc) where (g', id) = addFormula gr f

main = do
    (putStrLn . show) figure15

figure15 = g1
  where f = P( N ( P (AtomP "a"):/:P (AtomP "b")):<×>: (P (AtomP "b")))
        g0 = unfoldHypothesis f 0
        g1 = identifyNodes g0

figure18 = subUnfolded
  where sub = P ((N ((P (AtomP "np")):/:(N (AtomN "n")))):<×>: (P (AtomP "n")))
        tv  = N (N ((P (AtomP "np")):\:(P (AtomP "s"))):/:(P (AtomP "np")))
        det = N ((P (AtomP "np")):/:(P (AtomP "n")))
        noun = P (AtomP "n")
        goal = N (AtomN "s")
        subUnfolded = unfoldHypothesis sub 0


exampleGraph = g2
  where
    g0 = Map.empty
    f =  P (N (P (AtomP "B") :/: P (AtomP "A")) :<×>: P (AtomP "C"))
    idLink = (Active 0) :|: (Active 1)
    nodeInfo :: NodeInfo
    nodeInfo = Node f (Va (Variable "0")) (Just idLink) Nothing
    g1 = insertNodes g0 [(0, nodeInfo)]
    g2 = unfoldHypothesis f 0
    