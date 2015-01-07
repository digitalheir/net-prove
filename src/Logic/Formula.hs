module Logic.Formula where
import Logic.Link
import Logic.Node
import Logic.ProofStructure

-- Typeclass for formulae
class Formulae f where
  -- Unfold functions to create proof nets. Return tuple gives the necessary ingredients to expand a given formula into a link, in order: link type, list of premises, list of conclusions, zero-based index of main formula ((0,n) for premise (1,n) for conclusion)
  unfoldHypothesis :: f -> (LinkType, [f], [f], (Int, Int))
  unfoldConclusion :: f -> (LinkType, [f], [f], (Int, Int))