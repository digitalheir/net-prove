module Logic.ProofStructure
( ProofStructure,
-- constructProofStructure,
 fromFormulas,
 formulas,
 links,
 hypotheses
)
where
import qualified Logic.Link as Link
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

type FormulaMap = Map.Map Link.FormulaIdentifier

-- Moortgat & Moot 2012:
--   *Definition 2.2.* A proof structure <S, L> is a finite set of formula occurrences S
--   and a set of links L [...] such that
--   • each formula is at most once the premise of a link,
--   • each formula is at most once the conclusion of a link

-- Inductive constructor for a proof structure:
-- TODO experiment if this works better than the set approach
data ProofStructureInd f = Empty | S (Link.Link) (ProofStructureInd f) deriving (Show)

-- Constructor for a proof structure using lists:
data ProofStructure f = ProofStructure {formulas :: FormulaMap f, links :: [Link.Link], acc :: Integer} deriving (Show, Eq)

empty :: ProofStructure f
empty = ProofStructure Map.empty [] 0

addFormula :: (Eq f, Show f) => (Link.FormulaIdentifier, f) -> ProofStructure f -> ProofStructure f
addFormula (id, formula) (ProofStructure formMap ls i) =
  if isNothing maybeVal
  then (ProofStructure newMap ls i) -- Return new proof structure
  else
    if formula == fromJust maybeVal
    then (ProofStructure formMap ls i) --Structure already contains formula, return same structure
    else error ("Proof structure already has a formula with id "++(show id))
  where
    maybeVal = (id `Map.lookup` formMap)
    newMap = Map.insert id formula formMap

fromFormulas :: [f] -> ProofStructure f
fromFormulas formulas = ProofStructure formulaMap [] i
  where (formulaMap, i) = createFormulaMap formulas Map.empty 0
        createFormulaMap [] map i = (map, i)
        createFormulaMap (f:fs) map i = createFormulaMap fs (Map.insert i f map) (i+1)

-- TODO Each formula should be at most once the premise of a link and at most once the conclusion of a link; check for this
addLink :: Link.Link -> (ProofStructure f) -> (ProofStructure f)
addLink l (ProofStructure f ls acc) =
  if hasFormulaIds (Link.premises l) structure && hasFormulaIds (Link.conclusions l) structure && hasFormulaIds [Link.mainFormula l] structure
  then (ProofStructure f (l:ls) acc)
  else error "Formula identifier specified that could not be found in proof structure"
  where
    structure = ProofStructure f ls acc
    hasFormulaIds [] s = True
    hasFormulaIds (f:fs) s = if s `containsFormula` f then hasFormulaIds fs s else False--error "Could not find id "++(show f)++" in formula map for the given proof structure"


containsFormula (ProofStructure functions ls acc) id = id `Map.member` functions

-- M&M 2012, p7: "Formulas which are not the conclusion of any link are called the hypotheses of the proof structure."
-- Complexity: O(n), for n is the number of formulas in this proof structure
hypotheses s = getProofHypotheses (Map.keys (formulas s))
  where
   getProofHypotheses = foldl (addIfNotConclusion (links s)) []
   addIfNotConclusion :: [Link.Link] -> [Link.FormulaIdentifier] -> Link.FormulaIdentifier -> [Link.FormulaIdentifier]
   addIfNotConclusion links acc formula =
     if not (formula `Link.isConclusionOfAnyLink` links)
     then (formula:acc)
     else acc


-- M&M 2012, p7: "Formulas which are not the premise of any link are called the conclusions of the proof structure."
-- Complexity: O(n), for n is the number of formulas in this proof structure
conclusions s = getProofConclusions (Map.keys (formulas s))
  where
   getProofConclusions = foldl (addIfNotPremise (links s)) []
   addIfNotPremise :: [Link.Link] -> [Link.FormulaIdentifier] -> Link.FormulaIdentifier -> [Link.FormulaIdentifier]
   addIfNotPremise links acc formula =
     if not (formula `Link.isPremiseOfAnyLink` links)
     then (formula:acc)
     else acc









-- NOTE: Each formula should be at most once the premise of a link and at most once the conclusion of a link
--constructProofStructure :: (Eq f) => [f] -> [Link.Link] -> ProofStructure f
--constructProofStructure :: (Eq f) => [f] -> [Link.Link] -> ProofStructure f
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
