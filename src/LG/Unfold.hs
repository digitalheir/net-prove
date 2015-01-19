module LG.Unfold where
import LG.Graph

---------------------
--- UNFOLD HYPOTHESIS
---------------------
unfoldHypothesis f@(P _) idCounter = unfoldHypothesis' f term Nothing idCounter
  where term = (Va (Variable (show idCounter)))
unfoldHypothesis f@(N _) idCounter = unfoldHypothesis' f term Nothing idCounter
  where term = (Ev (Covariable (show idCounter)))

unfoldHypothesis' f defaultTerm l idCounter = (firstId, nodes++aNodes++bNodes, bCount)
    where -- Common to all unfoldings: first create an axioma link
          firstId  = idCounter+0
          mainId   = idCounter+1
          newCount = idCounter+2
          axLink   = Just ((Active firstId) :|: (Active mainId))
          nodes    = [firstId:@ (Node f defaultTerm axLink l),
                      mainId :@ (Node f unfoldTerm  operatorLink axLink)]
          -- Formula-specific unfold info
          (unfoldTerm, operatorLink, aNodes, bNodes, bCount) = getUnfoldInfoH f mainId newCount

-- Atoms: terminal nodes for unfolding
getUnfoldInfoH f@(P (AtomP _)) mainId newCount = (Va (Variable   (show mainId)), Nothing, [], [], newCount)
getUnfoldInfoH f@(N (AtomN _)) mainId newCount = (Ev (Covariable (show mainId)), Nothing, [], [], newCount)

-- L/
getUnfoldInfoH f@(N (b:/:a)) mainId newCount = (term, operatorLink, aNodes, bNodes, bCount)
    where term     = Ev (bVar :/ aVar)
          aVar     = Variable (show aId)
          bVar     = Covariable (show bId)
          aTerm    = (Va aVar)
          bTerm    = (Ev bVar)
          link = [MainT (mainId), Active aId]
                             :○:
                        [Active bId]
          operatorLink = Just link
          (aId, aNodes, aCount) = unfoldConclusion' a aTerm operatorLink newCount
          (bId, bNodes, bCount) = unfoldHypothesis' b bTerm operatorLink aCount
-- L⊗
getUnfoldInfoH f@(P (a :<×>: b)) mainId idCount = (mainTerm, operatorLink, aNodes, bNodes, bCount)
  where mainTerm     = (Va (Variable (show mainId)))
        aTerm = (Va (Variable (show aId)))
        bTerm = (Va (Variable (show bId)))
        link         =      [MainT (mainId)]
                                     :●:
                           [Active aId, Active bId]
        operatorLink = Just link
        (aId, aNodes, aCount) = unfoldHypothesis' a aTerm operatorLink idCount
        (bId, bNodes, bCount) = unfoldHypothesis' b bTerm operatorLink aCount

-- L\
getUnfoldInfoH f@(N (a :\: b)) mainId idCount = (mainTerm, operatorLink, aNodes, bNodes, bCount)
  where
    mainTerm = Ev (aVar :\ bVar)
    aVar = (Variable   (show aId))
    bVar = (Covariable (show bId))
    link        = [Active aId, MainT (mainId)]
                               :○:
                          [Active bId]
    operatorLink = Just link
    (aId, aNodes, aCount) = unfoldConclusion' a (Va aVar) operatorLink idCount
    (bId, bNodes, bCount) = unfoldHypothesis' b (Ev bVar) operatorLink aCount

-- L⊘
getUnfoldInfoH mainFormula@(P (b :</>: a)) mainId idCount = (mainTerm, operatorLink, aNodes, bNodes, bCount)
  where
    -- Node terms, based on their node ids
    mainTerm = Ev (Covariable (show mainId))
    aTerm = Va (Variable   (show aId))
    bTerm = Ev (Covariable (show bId))
    -- Operator link
    link = [MainT (mainId), Active aId]
                         :●:
                     [Active bId]
    operatorLink = Just link
    (aId, aNodes, aCount) = unfoldConclusion' a aTerm operatorLink idCount
    (bId, bNodes, bCount) = unfoldHypothesis' b bTerm operatorLink aCount

-- L⊕
getUnfoldInfoH mainFormula@(N (a :<+>: b)) mainId idCount = (mainTerm, operatorLink, aNodes, bNodes, bCount)
  where 
    mainTerm = Ev (aVar :<+> bVar)
    aVar = (Covariable (show aId))
    bVar = (Covariable (show bId))
    aTerm = Ev aVar
    bTerm = Ev bVar
    -- Operator link
    link         =      [MainT (mainId)]
                               :○:
                     [Active aId, Active bId]
    operatorLink = Just link
    (aId, aNodes, aCount) = unfoldConclusion' a aTerm operatorLink idCount
    (bId, bNodes, bCount) = unfoldHypothesis' b bTerm operatorLink aCount

-- L<\>
getUnfoldInfoH mainFormula@(P (a :<\>: b)) mainId idCount = (mainTerm, operatorLink, aNodes, bNodes, bCount)
  where
    -- Node terms, based on their node ids
    mainTerm = Va (aVar :<\> bVar)
    aVar = (Covariable (show aId))
    bVar = (Variable   (show bId))
    aTerm = Ev aVar
    bTerm = Va bVar
    -- Operator link
    link         = [Active aId, MainT (mainId)]
                               :●:
                           [Active bId]
    operatorLink = Just link
    (aId, aNodes, aCount) = unfoldConclusion' a aTerm operatorLink idCount
    (bId, bNodes, bCount) = unfoldHypothesis' b bTerm operatorLink aCount

---------------------
--- UNFOLD CONCLUSION
---------------------
unfoldConclusion f@(P _) idCounter = unfoldConclusion' f term Nothing idCounter
  where term = (Va (Variable (show idCounter)))
unfoldConclusion f@(N _) idCounter = unfoldConclusion' f term Nothing idCounter
  where term = (Ev (Covariable (show idCounter)))

unfoldConclusion' f defaultTerm l idCounter = (firstId, nodes++aNodes++bNodes, bCount)
    where -- Common to all unfoldings: first create an axioma link
          firstId  = idCounter+0
          mainId   = idCounter+1
          newCount = idCounter+2
          axLink   = Just ((Active mainId) :|: (Active firstId))
          nodes    = [firstId:@ (Node f defaultTerm l axLink),
                      mainId :@ (Node f unfoldTerm  axLink operatorLink)]
          -- Formula-specific unfold info
          (unfoldTerm, operatorLink, aNodes, bNodes, bCount) = getUnfoldInfoC f mainId newCount

-- Atoms: terminal nodes for unfolding
getUnfoldInfoC f@(P (AtomP _)) mainId idCount = (Va (Variable   (show mainId)), Nothing, [], [], idCount)
getUnfoldInfoC f@(N (AtomN _)) mainId idCount = (Ev (Covariable (show mainId)), Nothing, [], [], idCount)

-- R/
getUnfoldInfoC f@(N (b:/:a)) mainId idCounter = (mainTerm, operatorLink, aNodes, bNodes, bCount)
    where firstId  = idCounter+0
          mainId   = idCounter+1
          -- Terms, derived from node ids
          mainTerm = Ev (bVar :/ aVar)
          aVar     = Variable   (show aId)
          bVar     = Covariable (show bId)
          aTerm    = Va aVar
          bTerm    = Ev bVar
          -- Operator link
          link     =      [Active bId]
                               :●:
                    [MainT (mainId), Active aId]
          operatorLink = Just link
          (bId, bNodes, bCount) = unfoldConclusion' b bTerm operatorLink aCount
          (aId, aNodes, aCount) = unfoldHypothesis' a aTerm operatorLink (idCounter+2)

-- R⊗
getUnfoldInfoC f@(P (b:<×>:a)) mainId idCounter = (mainTerm, operatorLink, aNodes, bNodes, bCount)
    where firstId  = idCounter+0
          mainId   = idCounter+1
          -- Terms, derived from node ids
          mainTerm = Va (aVar :<×> bVar)
          aVar     = Variable (show aId)
          bVar     = Variable (show bId)
          aTerm    = Va aVar
          bTerm :: NodeTerm
          bTerm    = Va bVar
          -- Operator link
          link     =[Active aId, Active bId]
                               :○:
                          [MainT (mainId)]
          operatorLink = Just link
          (aId, aNodes, aCount) = unfoldConclusion' a aTerm operatorLink (idCounter+2)
          (bId, bNodes, bCount) = unfoldConclusion' b bTerm operatorLink aCount

-- R\
getUnfoldInfoC f@(N (b:\:a)) mainId idCounter = (mainTerm, operatorLink, aNodes, bNodes, bCount)
    where firstId  = idCounter+0
          mainId   = idCounter+1
          -- Terms, derived from node ids
          mainTerm = Ev (bVar :/ aVar)
          aVar     = Variable   (show aId)
          bVar     = Covariable (show bId)
          aTerm    = Va aVar
          bTerm    = Ev bVar
          -- Operator link
          link     =      [Active bId]
                               :●:
                    [Active aId, MainT (mainId)]
          operatorLink = Just link
          (aId, aNodes, aCount) = unfoldHypothesis' a (Va aVar) operatorLink (idCounter+2)
          (bId, bNodes, bCount) = unfoldConclusion' b (Ev bVar) operatorLink aCount

-- R⊘
getUnfoldInfoC f@(P (b:</>:a)) mainId idCounter = (mainTerm, operatorLink, aNodes, bNodes, bCount)
    where firstId  = idCounter+0
          mainId   = idCounter+1
          -- Terms, derived from node ids
          mainTerm = Va (bVar :</> aVar)
          aVar     = Covariable (show aId)
          bVar     = Variable   (show bId)
          aTerm    = Ev aVar
          bTerm    = Va bVar
          -- Operator link
          operatorLink = Just ([Active bId]
                                   :○:
                        [MainT (mainId), Active aId])
          (aId, aNodes, aCount) = unfoldHypothesis' a aTerm operatorLink (idCounter+2)
          (bId, bNodes, bCount) = unfoldConclusion' b bTerm operatorLink aCount

-- R⊕
getUnfoldInfoC f@(N (a:<+>:b)) mainId idCounter = (mainTerm, operatorLink, aNodes, bNodes, bCount)
    where firstId  = idCounter+0
          mainId   = idCounter+1
          -- Terms, derived from node ids
          mainTerm = Ev (bVar :<+> aVar)
          aVar     = Covariable (show aId)
          bVar     = Covariable (show bId)
          aTerm    = Ev aVar
          bTerm    = Ev bVar
          -- Operator link
          operatorLink = Just ([Active aId, Active bId]
                                   :●:
                             [MainT (mainId)])
          (aId, aNodes, aCount) = unfoldConclusion' a aTerm operatorLink (idCounter+2)
          (bId, bNodes, bCount) = unfoldConclusion' b bTerm operatorLink aCount

-- R<\>
getUnfoldInfoC f@(P (a:<\>:b)) mainId idCounter = (mainTerm, operatorLink, aNodes, bNodes, bCount)
    where firstId  = idCounter+0
          mainId   = idCounter+1
          -- Terms, derived from node ids
          mainTerm = Va (aVar :<\> bVar)
          aVar     = Covariable (show aId)
          bVar     = Variable   (show bId)
          aTerm    = Ev aVar
          bTerm    = Va bVar
          -- Operator link
          operatorLink = Just ([Active bId]
                                   :○:
                        [Active aId, MainT (mainId)])
          (aId, aNodes, aCount) = unfoldHypothesis' a aTerm operatorLink (idCounter+2)
          (bId, bNodes, bCount) = unfoldConclusion' b bTerm operatorLink aCount