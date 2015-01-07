module Logic.LG where
import qualified Logic.Link as Link
import Logic.Formula
import Logic.ProofStructure

-- Formula language of Lambek Grishin calculus (LG); see Moortgat & Moot 2012, p. 1-2
data Formula a = Atom a |         -- NP, S, etc.
  (:*:) (Formula a) (Formula a) | -- ⊗ (Prod)
  (:\\) (Formula a) (Formula a) | -- \ (LDiv)
  (://) (Formula a) (Formula a) | -- / (RDiv)
  (:+:) (Formula a) (Formula a) | -- ⊕ (Sum)
  (:-\) (Formula a) (Formula a) | -- ⃠ (LSub)
  (:-/) (Formula a) (Formula a)   -- ⊘ (RSub)
  deriving (Eq, Ord)
instance (Show a) => Show (Formula a) where
  show (Atom a) = show a
  show (a :// b) = "("++(show a) ++ " /" ++ (show b) ++ ")" -- /
  show (a :*: b) = "("++(show a) ++ " [x] " ++ (show b) ++ ")" -- ⊗
  show (a :\\ b) = "("++(show a) ++ "\\ " ++ (show b) ++ ")" -- \
  show (a :-/ b) = "("++(show a) ++ " [/]" ++ (show b) ++ ")" -- ⊘
  show (a :+: b) = "("++(show a) ++ " [+] " ++ (show b) ++ ")" -- ⊕
  show (a :-\ b) = "("++(show a) ++ "[\\] " ++ (show b) ++ ")" -- ⃠

instance (Eq a) => Formulae (Formula a) where
  -- Rules for creating Links from complex formulae. See also M&M 2012, p6
  --- Hypothesis
--  unfoldHypothesis :: (Eq a) => Formula a -> (Link.LinkType, [Formula a], [Formula a], Formula a)
  ---- Fusion connectives (Hypothesis)
  unfoldHypothesis (a :// b) = (Link.Tensor,   [a :// b, b], [a],    (0,0)) -- L/
  unfoldHypothesis (a :*: b) = (Link.CoTensor, [a :*: b],    [a, b], (0,0)) -- L⊗
  unfoldHypothesis (b :\\ a) = (Link.Tensor,   [b :\\ a, b], [a],    (0,0)) -- L\
  ---- Fission connectives (Hypothesis)
  unfoldHypothesis (a :-/ b) = (Link.CoTensor, [a :-/ b, b], [a],    (0,0)) -- L⊘
  unfoldHypothesis (a :+: b) = (Link.Tensor,   [a :+: b],    [a, b], (0,0)) -- L⊕
  unfoldHypothesis (b :-\ a) = (Link.CoTensor, [b, b :\\ a], [a],    (0,1)) -- L⃠
  -- We can't unfold atomic formulae
  unfoldHypothesis (Atom a) = (Link.NoLink,       [Atom a], [], (0,0))
  ---
  --- Conclusion
--  unfoldConclusion :: (Eq a) => (Link.LinkType, Formula a, Formula a, Formula a)
  ---- Fusion connectives (Conclusion)
  unfoldConclusion (a :// b) = (Link.CoTensor, [a], [a :// b, b], (1,0)) -- R/
  unfoldConclusion (a :*: b) = (Link.Tensor,   [a, b], [a :*: b], (1,0)) -- R⊗
  unfoldConclusion (b :\\ a) = (Link.CoTensor, [a], [b, b :\\ a], (1,1)) -- R\
  ---- Fission connectives (Conclusion)
  unfoldConclusion (a :-/ b) = (Link.Tensor,   [a], [a :-/ b, b], (1,0)) -- R⊘
  unfoldConclusion (a :+: b) = (Link.CoTensor, [a, b], [a :+: b], (1,0)) -- R⊕
  unfoldConclusion (b :-\ a) = (Link.Tensor,   [a], [b, b :-\ a], (1,1)) -- R⃠
  -- We can't unfold atomic formulae
  unfoldConclusion (Atom a) = (Link.NoLink,       [], [Atom a], (1,0))

--- Unfold examples
--unfoldExamples = [
--  unfoldHypothesis ((Atom "A") :// (Atom "B")), -- L/
--  unfoldHypothesis ((Atom "A") :*: (Atom "B")), -- L⊗
--  unfoldHypothesis ((Atom "B") :\\ (Atom "A")), -- L\
--  unfoldHypothesis ((Atom "A") :-/ (Atom "B")), -- L⊘
--  unfoldHypothesis ((Atom "A") :+: (Atom "B")), -- L⊕
--  unfoldHypothesis ((Atom "B") :-\ (Atom "A")), -- L⃠
--  unfoldConclusion ((Atom "A") :// (Atom "B")), -- R/
--  unfoldConclusion ((Atom "A") :*: (Atom "B")), -- R⊗
--  unfoldConclusion ((Atom "B") :\\ (Atom "A")), -- R\
--  unfoldConclusion ((Atom "A") :-/ (Atom "B")), -- R⊘
--  unfoldConclusion ((Atom "A") :+: (Atom "B")), -- R⊕
--  unfoldConclusion ((Atom "B") :-\ (Atom "A"))] -- R⃠
--