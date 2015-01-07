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
--
-- NOTE: We could think of sets instead of lists for our premises and conclusions
data LinkType = Tensor | CoTensor | NoLink deriving (Show, Eq)
data LinkDirection = LeftLink | RightLink | DirectionLess deriving (Show, Eq)
data Link a = Link {
                     linkType :: LinkType,
                     premises :: [Node a],
                     conclusions :: [Node a],
                     mainFormula :: Node a
                   }
            deriving (Eq)

instance (Show a) => Show (Link a) where
    show l =
      (show (premises l)) ++
      "\n" ++
      "      "++
      (showLinkType (linkType l))++
      "\n" ++
      (show (conclusions l))
      where
        showLinkType lt = if lt == Tensor then "O" else if lt == CoTensor then "*" else "-"


constructLink :: (Eq a) => LinkType -> [Node a] -> [Node a] -> (Node a) -> (Link a)
constructLink linkType premises conclusions mainFormula =
  if (ld == LeftLink) || (ld == RightLink) || (linkDirection link == DirectionLess) -- trivial, but this performs an error check
  then link
  else error "Main formula must be an element of premises or an element of conclusions or nil"
  where link = Link linkType premises conclusions mainFormula
        ld = linkDirection link

-- Returns whether formula has a main link assigned
hasMainFormula :: (Eq a) => (Link a) -> Bool
hasMainFormula link =
  if (mainFormula link) == NilNode
  then False
  else True

-- Moortgat & Moot 2012, p5: "In case m[ainFormula] is a member of p[remises] we speak of a left link
-- (corresponding to the left rules of the sequent calculus, where the main formula of the link occurs in the antecedent)"
isLeftLink :: (Eq a) => (Link a) -> Bool
isLeftLink link = (mainFormula link) `elem` (premises link)

-- "...and in case m[ainFormula] is a member of c[onclusions] we speak of a right link."
isRightLink :: (Eq a) => (Link a) -> Bool
isRightLink link = (mainFormula link) `elem` (conclusions link)

linkDirection :: (Eq a) => (Link a) -> LinkDirection
linkDirection link =
  if not (hasMainFormula link)
  then DirectionLess
  else
    if isLeftLink link
    then LeftLink
    else
      if isRightLink link
      then RightLink
      else error "Link is neither a left link nor a right link: mainFormula must be a member of the premises, or the conclusions, or the node equivalent of nil"


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

