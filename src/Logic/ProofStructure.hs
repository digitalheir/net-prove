module Logic.ProofStructure
--( ProofStructure,
---- constructProofStructure,
---- fromFormulas,
--addFormula,
-- formulas,
-- links,
-- hypotheses
--)
where
import Logic.Node
import qualified Logic.Link as Link
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

-- A map that identifies nodes by their identifiers. We need this to distinguish between various occurrences of the same formula in a proof structure
type FormulaMap = Map.Map NodeIdentifier

-- Moortgat & Moot 2012:
--   *Definition 2.2.* A proof structure <S, L> is a finite set of formula occurrences S
--   and a set of links L [...] such that
--   • each formula is at most once the premise of a link,
--   • each formula is at most once the conclusion of a link

-- Inductive constructor for a proof structure:
-- TODO experiment if this works better than the set approach
data ProofStructureInd f = Empty | S (Link.Link f) (ProofStructureInd f) deriving (Show)

-- Constructor for a proof structure using lists:
data ProofStructure f = ProofStructure {
                                         formulas :: FormulaMap (Node f), -- TODO we can use a vector for this, so we can add/query indices in O(1) instead of O(log n)
                                         links :: [Link.Link f],
                                         count :: Int -- Think of this number as 'next node index / next node slot', or as a counter of how many nodes we have added in total (including those without references in the formula map)
                                       } deriving (Show, Eq)

emptyProofStructure :: ProofStructure f
emptyProofStructure = ProofStructure Map.empty [] 0

-- Adds given formulas to given proof structure, creating one node for each formula
-- NB: two instances of the same formula will be two separate nodes
-- Returns: A pair of which the first element is the resulting proof structure, and the second element is an array of the new nodes that were added
--addFormulas :: (Eq f) => ProofStructure f -> [f] -> (ProofStructure f, [Node f])
addFormulas s formulas = addFormulas' s formulas []
addFormulas' s []     acc = (s, acc)
addFormulas' s (f:fs) acc = addFormulas' newStructure fs ((newNode):acc)
  where
    addFormulaResult = (addFormula s f)
    newStructure = fst addFormulaResult
    newNode      = snd addFormulaResult

-- Adds given formula to given proof structure, creating a new node for the given formula
-- Returns: A pair of which the first element is the resulting proof structure, and the second element is the new node that was created
-- Complexity: O(log n) --TODO if we use a vector instead of map, can be O(1)
addFormula :: (Eq f) => ProofStructure f ->f ->  (ProofStructure f, Node f)
addFormula structure f =
  ((ProofStructure newMap (links structure) (nodeId+1)), node)
  where
    newMap   = Map.insert nodeId node oldMap
    node     = Node f nodeId
    oldMap   = formulas structure
    nodeId   = count    structure

-- Creates node for the next formula slot in given proof structure. If this node is among other to be added to the given proof structure, this one should be the first. (See addNode.)
-- Complexity: O(1)
createNode :: ProofStructure f -> f -> Node f
createNode s f = Node f (count s)

-- Adds given node to given proof structure.
-- WARNING: The node id *MUST* match the current count of the given proof structure, or this function will return an error
-- Complexity: O(log n) -- TODO use vector instead of map to make complexity O(1)
addNode :: ProofStructure f -> Node f -> ProofStructure f
addNode structure node =
    if nodeId node == index
    then (ProofStructure newMap (links structure) (index+1))
    else error ("Node id ("++(show (nodeId node))++") did not match proof structure slot number ("++(show index)++"). You should create a node with id "++(show (nodeId node)))
    where
      newMap   = Map.insert index node oldMap
      index    = count    structure
      oldMap   = formulas structure

--fromFormulas :: [f] -> ProofStructure f
--fromFormulas formulas = ProofStructure formulaMap [] i
--  where (formulaMap, i) = createFormulaMap formulas Map.empty 0
--        createFormulaMap [] map i = (map, i)
--        createFormulaMap (f:fs) map i = createFormulaMap fs (Map.insert i f map) (i+1)

--TODO Each formula should be at most once the premise of a link and at most once the conclusion of a link; check for this
addLink :: (Eq f) => (ProofStructure f) -> (Link.Link f) -> (ProofStructure f)
addLink structure link =
  if structure `hasNodes` (Link.premises    link) &&
     structure `hasNodes` (Link.conclusions link) &&
     structure `hasNodes` [Link.mainFormula link]
  then
    (ProofStructure
      (formulas structure)
      (link:(links structure))
      (count structure)
    )
  else error "Formula identifier specified that could not be found in proof structure"
  where
    hasNodes s []     = True
    hasNodes s (f:fs) = if s `containsNode` f
                        then hasNodes s fs
                        else False --error "Could not find id "++(show f)++" in formula map for the given proof structure"

-- Returns whether the given node is present in this proof structure
-- Complexity: O(log n)
containsNode :: (Eq f) => (ProofStructure f) -> (Node f) -> Bool
containsNode s node = (nodeId node) `Map.member` (formulas s)

-- M&M 2012, p7: "Formulas which are not the conclusion of any link are called the hypotheses of the proof structure."
-- Complexity: O(n), for n is the number of formulas in this proof structure
hypotheses s = getProofHypotheses (Map.elems (formulas s))
  where
   getProofHypotheses = foldl (addIfNotConclusion (links s)) []
   addIfNotConclusion :: (Eq f)=>[Link.Link f] -> [Node f] -> (Node f) -> [Node f]
   addIfNotConclusion links acc formula =
     if not (formula `Link.isConclusionOfAnyLink` links)
     then (formula:acc)
     else acc


-- M&M 2012, p7: "Formulas which are not the premise of any link are called the conclusions of the proof structure."
-- Complexity: O(n), for n is the number of formulas in this proof structure
-- Complexity: O(n), for n is the number of formulas in this proof structure
conclusions :: (Eq f) => ProofStructure f  -> [Node f]
conclusions s = getProofConclusions (Map.elems (formulas s))
  where
   getProofConclusions = foldl (addIfNotPremise (links s)) []
   addIfNotPremise :: (Eq f)=>[Link.Link f] -> [Node f] -> (Node f) -> [Node f]
   addIfNotPremise links acc formula =
     if not (formula `Link.isPremiseOfAnyLink` links)
     then (formula:acc)
     else acc

-- NOTE: Each formula should be at most once the premise of a link and at most once the conclusion of a link
--constructProofStructure :: (Eq f) => [f] -> [Link.Link f] -> ProofStructure f
--constructProofStructure :: (Eq f) => [f] -> [Link.Link f] -> ProofStructure f
--constructProofStructure formulas links =
--   if formulas `allOccurAtMostOnce` linkPremises && formulas `allOccurAtMostOnce` linkConclusions
--   then ProofStructure formulas links 0 -- Construct proof structure if it is well formed
--   else error
--   where
--     linkPremises = Link.getAllPremises links
--     linkConclusions = Link.getAllConclusions links
--
--     countIs1OrLess :: (Eq f) => f -> [f] -> Int->Bool
--     countIs1OrLess _ [] _ = True
--     countIs1OrLess f1 (f2:formulas) count =
--       if f1 == f2
--       then if count >=1
--            then False
--            else countIs1OrLess f1 formulas (count+1)
--       else countIs1OrLess f1 formulas count
--
--     allOccurAtMostOnce :: (Eq f) => [f] -> [f] -> Bool
--     allOccurAtMostOnce [] links = True
--     allOccurAtMostOnce (f:formulas) linkFormulas =
--       if countIs1OrLess f linkFormulas 0
--       then allOccurAtMostOnce formulas linkFormulas
--       else False
