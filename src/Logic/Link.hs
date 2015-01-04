module Logic.Link
(
LinkType(..),
Link,--(..), -- Do not expose type constructor for link; use constructor function
FormulaIdentifier(..),
constructLink,
isLeftLink,
isRightLink,
numberOfTentacles,
linkType,
premises ,
conclusions,
mainFormula,
getAllPremises,
getAllConclusions,
isConclusionOfAnyLink,
isPremiseOfAnyLink
) where

-- A map that identifies formulae by an integer. We need this to distinguish between various occurrences of the same formula in a proof structure
type FormulaIdentifier = Integer

-- Links: Tensor or cotensor links that connect a number of formulae.
--
-- Definition of Moortgat & Moot 2012, p5:
--
--   A link is a tuple <t, p, c, m> where
--   • t is the type of the link — tensor or cotensor
--   • p is the list of premises of the link,
--   • c is the list of conclusions of the link,
--   • m, the main vertex/formula of the link, is either a member of p, a member
--     of c or the constant “nil”

-- TODO Should two links be equal if they have the same arguments / conclusions, but in a different order (e.g., are premises and conclusions Sets)? I think so.
data LinkType = Tensor | CoTensor deriving (Show, Eq)
data Link = Link {
                       linkType :: LinkType,
                       premises :: [FormulaIdentifier],
                       conclusions :: [FormulaIdentifier],
                       mainFormula :: FormulaIdentifier
                     }
            deriving (Show, Eq)

-- TODO mainFormula may also be nil, but we should have a 'nullable' type f then, I don't know if there's a typeclass for that
constructLink :: LinkType -> [FormulaIdentifier] -> [FormulaIdentifier] -> FormulaIdentifier -> Link
constructLink linkType premises conclusions mainFormula =
  if mainFormula `elem` premises || mainFormula `elem` conclusions
  then Link linkType premises conclusions mainFormula
  else error "mainFormula must be a member of p, or c, or the node equivalent of nil"

-- Moortgat & Moot 2012, p5: "In case m[ainFormula] is a member of p[remises] we speak of a left link
-- (corresponding to the left rules of the sequent calculus, where the main formula of the link occurs in the antecedent)"
isLeftLink :: Link -> Bool
isLeftLink (Link _ premises _ mainFormula) = mainFormula `elem` premises

-- "...and in case m[ainFormula] is a member of c[onclusions] we speak of a right link."
isRightLink :: Link -> Bool
isRightLink (Link _ _ conclusions mainFormula) = mainFormula `elem` conclusions

-- Moortgat & Moot 2012, p5:
-- "when we need to refer to the connections between the central node and the vertices, we will call them its tentacles"
numberOfTentacles :: Link -> Int
numberOfTentacles (Link _ premises conclusions _) = length premises + length conclusions

--Some utility functions:
getAllPremises :: [Link] -> [FormulaIdentifier]
getAllPremises [] =  []
getAllPremises (link:links) =  premises link ++ getAllPremises links

getAllConclusions :: [Link] -> [FormulaIdentifier]
getAllConclusions [] =  []
getAllConclusions (link:links) =  conclusions link ++ getAllConclusions links

isConclusionOfAnyLink :: FormulaIdentifier -> [Link] -> Bool
isConclusionOfAnyLink formula [] = False
isConclusionOfAnyLink formula (l:links) =
  if formula `elem` (conclusions l)
  then True
  else isConclusionOfAnyLink formula links

isPremiseOfAnyLink :: FormulaIdentifier -> [Link] -> Bool
isPremiseOfAnyLink formula [] = False
isPremiseOfAnyLink formula (l:links) =
    if formula `elem` (premises l)
    then True
    else isPremiseOfAnyLink formula links

