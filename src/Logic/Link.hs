module Logic.Link
(
LinkType(..),
Link,--(..), -- Do not expose type constructor for link; use constructor function
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
import Logic.Node

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
data LinkType = Tensor | CoTensor | Nil deriving (Show, Eq)
data Link a = Link {
                       linkType :: LinkType,
                       premises :: [Node a],
                       conclusions :: [Node a],
                       mainFormula :: Node a
                     }
            deriving (Show, Eq)

-- TODO mainFormula may also be nil, but we should have a 'nullable' type f then, I don't know if there's a typeclass for that
constructLink :: (Eq a) => LinkType -> [Node a] -> [Node a] -> (Node a) -> (Link a)
constructLink linkType premises conclusions mainFormula =
  if mainFormula `elem` premises || mainFormula `elem` conclusions
  then Link linkType premises conclusions mainFormula
  else error "mainFormula must be a member of p, or c, or the node equivalent of nil"

-- Moortgat & Moot 2012, p5: "In case m[ainFormula] is a member of p[remises] we speak of a left link
-- (corresponding to the left rules of the sequent calculus, where the main formula of the link occurs in the antecedent)"
isLeftLink :: (Eq a) => (Link a) -> Bool
isLeftLink link = (mainFormula link) `elem` (premises link)

-- "...and in case m[ainFormula] is a member of c[onclusions] we speak of a right link."
isRightLink :: (Eq a) => (Link a) -> Bool
isRightLink link = (mainFormula link) `elem` (conclusions link)

-- Moortgat & Moot 2012, p5:
-- "when we need to refer to the connections between the central node and the vertices, we will call them its tentacles"
numberOfTentacles :: (Link a) -> Int
numberOfTentacles link = length (premises link) + length (conclusions link)

--Some utility functions:
getAllPremises :: [Link a] -> [Node a]
getAllPremises [] =  []
getAllPremises (link:links) =  premises link ++ getAllPremises links

getAllConclusions :: [Link a] -> [Node a]
getAllConclusions [] =  []
getAllConclusions (link:links) =  conclusions link ++ getAllConclusions links

isConclusionOfAnyLink :: (Eq a) => (Node a) -> [Link a] -> Bool
isConclusionOfAnyLink formula [] = False
isConclusionOfAnyLink formula (l:links) =
  if formula `elem` (conclusions l)
  then True
  else isConclusionOfAnyLink formula links

isPremiseOfAnyLink :: (Eq a) => (Node a) -> [Link a] -> Bool
isPremiseOfAnyLink formula [] = False
isPremiseOfAnyLink formula (l:links) =
    if formula `elem` (premises l)
    then True
    else isPremiseOfAnyLink formula links

