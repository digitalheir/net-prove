module Main where
import Logic.ProofStructure
import Logic.LG
import qualified Logic.Link as Link
import Logic.Formula

--exampleJudgment :: ProofStructure (Formula String)
exampleJudgment =
  s'
  where
    e = emptyProofStructure
    inputForms = [
      ((Atom "np")  :// (Atom "n")) :*: (Atom "n"),
      ((Atom "np")  :\\ (Atom "s")) :// (Atom "np"),
      (Atom "np")   :// (Atom "n"),
      (Atom "n")]
    s' = unfoldInputs e inputForms
--    s'' = unfoldOutputs e outputForms
    outputForms = [Atom "s"]

unfoldInputs :: (Formulae t, Eq t) => ProofStructure t -> [t] -> ProofStructure t
unfoldInputs s [] = s
unfoldInputs s (f:fs) = unfoldInputs s' fs
  where s' = unfoldHypothesisInStructure s f

-- (np / n) ⊗ n, (np \ s) / np, np / n, n ⇒ s

unfoldHypothesisInStructure :: (Formulae t, Eq t) => ProofStructure t -> t -> ProofStructure t
unfoldHypothesisInStructure s f = finalStructure
  where
    (linkType, premises, conclusions, (ix1, ix2)) = unfoldHypothesis f
    (s' , premiseNodes   ) = addFormulas s  premises
    (s'', conclusionNodes) = addFormulas s' conclusions
    mainFormula = if ix1 == 0 then premiseNodes !! ix2 else conclusionNodes !! ix2
    link = Link.constructLink linkType premiseNodes conclusionNodes mainFormula
    finalStructure = if linkType == Link.Nil
                     then addNode s mainFormula -- Our new structure is just the old structured with the atomic formula added on
                     else addLink s'' link         -- Our new structure is the old structure plus some new formulas and a new link
