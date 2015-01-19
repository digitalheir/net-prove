module LG.Identify where
import LG.Graph
import qualified Data.Map as Map

--identifyNodes :: CompositionGraph -> [CompositionGraph]
identifyNodes g0 = possibleCombinations
  where leafs = leafNodes g0
        possibleCombinations = [(i,j) | i@(id1:@(Node iFormula _ iPrem iConc)) <- leafs,
                                        j@(id2:@(Node jFormula _ jPrem jConc)) <- leafs,
                                        not(i==j),
                                        iFormula==jFormula,
                                        (iPrem==Nothing && jConc==Nothing) || (iConc==Nothing && jPrem==Nothing)]

leafNodes :: CompositionGraph -> [Occurrence NodeInfo]
leafNodes g = map pairToOccurrence (Map.toList (Map.filter isLeafNode g))
  where isLeafNode (Node _ _ Nothing _) = True
        isLeafNode (Node _ _ _ Nothing) = True
        isLeafNode _ = False
        pairToOccurrence (id, occurrence) = id :@ occurrence