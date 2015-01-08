module Main where
import Logic.ProofStructure
import Logic.LG
import qualified Logic.Link as Link
import Logic.Formula

--exampleJudgment :: ProofStructure (Formula String)
-- (np / n) ⊗ n, (np \ s) / np, np / n, n ⇒ s
exampleJudgment =
  s'
  where
    e = emptyProofStructure
    inputForms = [
      ((Atom "np") :// (Atom "n")) :*: (Atom "n"),
      ((Atom "np") :\\ (Atom "s")) :// (Atom "np"),
      (Atom "np")  :// (Atom "n"),
      (Atom "n")]
    s'  = unfoldInputs e inputForms
    s'' = unfoldOutputs s' outputForms
    outputForms = [Atom "s"]
    unfoldInputs s [] = s
    unfoldInputs s (f:fs)  = unfoldInputs (addUnfoldingToStructure s (unfoldHypothesis f)) fs
    unfoldOutputs s [] = s
    unfoldOutputs s (f:fs) = unfoldOutputs (addUnfoldingToStructure s (unfoldConclusion f)) fs

addUnfoldingToStructure :: (Eq f) => ProofStructure f -> LinkInfo f -> ProofStructure f
addUnfoldingToStructure s (linkType, premises, conclusions, (ix1, ix2)) =
  if linkType == Link.NoLink
  then addNode s mainFormula -- Our new structure is just the old structured with the atomic formula added on
  else addLink s'' link      -- Our new structure is the old structure plus some new formulas and a new link
  where
    (s' , premiseNodes   ) = addFormulas s  premises
    (s'', conclusionNodes) = addFormulas s' conclusions
    mainFormula = if ix1 == 0 then premiseNodes !! ix2 else conclusionNodes !! ix2
    link = Link.constructLink linkType premiseNodes conclusionNodes mainFormula