import Data.List (nub, sort)

type Transition = (Int, Char, Int)
type Automaton = (Int, String, [Transition], Int, [Int])

ex1 :: Automaton
ex1 = (3, "ab", [(0,'a',1), (0,'b',0), (1,'a',1), (1,'b',2), (2,'a',1), (2,'b',0)], 0, [2])

ex2 :: Automaton
ex2 = (3, "ab", [(0,'a',1), (0,'a',0), (0,'b',0), (1,'b',2)], 0, [2])

ex3 :: Automaton
ex3 = (3, "ab", [(0,'a',1), (0,'b',0), (1,'a',1), (1,'b',2), (2,'a',1)], 0, [2])

ex4 :: Automaton
ex4 = (3,"ab",[(0,'a',1),(0,'b',0),(1,'a',1),(1,'b',2),(2,'a',1),(2,'b',0)],0,[2])


isDeterministic :: Automaton -> Bool
isDeterministic (n, sigma, delta, q0, f) = all checkState [0..n-1]
    where
        -- check all symbols for each state q
        checkState q = all (checkSymbol q) sigma
        -- check if there is exactly one transition for each state q and symbol a
        checkSymbol q a = length (filter (matchesTransition q a) delta) == 1
        -- check if transition starts in state q with symbol a
        matchesTransition q a (q1, a1, q2) = q1 == q && a1 == a


isAccepting :: Automaton -> String -> Bool
isAccepting (n, sigma, delta, q0, f)
    | isDeterministic (n, sigma, delta, q0, f) = isAcceptingInner (n, sigma, delta, q0, f)
    | otherwise = isAcceptingInner (convert (n, sigma, delta, q0, f))

isAcceptingInner :: Automaton -> String -> Bool
isAcceptingInner (n, sigma, delta, q0, f) w = processString q0 w `elem` f
    where
        -- process string by applying transitions
        processString q [] = q
        -- apply transition for each symbol in the string
        processString q (a:as) = processString (transition q a) as
        -- get next state for transition
        transition q a = head (nextStates q a)
        -- get all next states for transition
        nextStates q a = [q2 | (q1, a1, q2) <- delta, q1 == q && a1 == a]


convert :: Automaton -> Automaton
convert (n, sigma, delta, q0, f)
    | isDeterministic (n, sigma, delta, q0, f) = (n, sigma, delta, q0, f)
    | otherwise = convertInner (n, sigma, delta, q0, f)

convertInner :: Automaton -> Automaton
convertInner (n, sigma, delta, q0, f) = (n', sigma, dfaDelta, 0, dfaFinals)
    where
        -- State sets in DFA correspond to subsets of NFA states
        dfaStates = generateDFAStates [[q0]] [] sigma delta
        dfaStateIndex = zip dfaStates [0..]
        n' = length dfaStates

        -- Transitions for the DFA
        dfaDelta = [(dfaStateIndexLookup src, a, dfaStateIndexLookup dest)
                    | src <- dfaStates,
                      a <- sigma,
                      let dest = sort $ nub [q2 | q1 <- src, (q1', a', q2) <- delta, q1 == q1' && a == a']]

        -- DFA final states
        dfaFinals = [j | (qs, j) <- dfaStateIndex, any (`elem` f) qs]

        -- Lookup DFA state index for a given set of NFA states
        dfaStateIndexLookup qs = case filter ((== qs) . fst) dfaStateIndex of
                                   [] -> error ("State set not found: " ++ show qs)
                                   ((_, j):_) -> j

-- Generate all reachable DFA states
generateDFAStates :: [[Int]] -> [[Int]] -> String -> [Transition] -> [[Int]]
generateDFAStates [] visited _ _ = visited
generateDFAStates (current:queue) visited sigma delta
    | current `elem` visited = generateDFAStates queue visited sigma delta
    | otherwise = generateDFAStates (queue ++ newStates) (visited ++ [current]) sigma delta
    where
        -- Compute all possible new DFA states from the current state using sigma
        newStates = [sort $ nub [q2 | q1 <- current, (q1', a, q2) <- delta, q1 == q1' && a == s] | s <- sigma]


-- Second homework
data RegExpr = Epsilon
             | Symbol Char
             | Iteration RegExpr
             | Concat RegExpr RegExpr
             | Alter RegExpr RegExpr
                deriving (Eq, Show)

reg1 :: RegExpr 
reg1 = Concat (Concat (Iteration (Alter (Symbol 'a') (Symbol 'b'))) (Symbol 'a')) (Symbol 'b')

regCon :: RegExpr
regCon = Concat(Concat(Concat (Symbol 'a') (Symbol 'b')) (Symbol 'c')) (Symbol 'd')

regAlt :: RegExpr
regAlt = Alter(Alter(Alter (Symbol 'a') (Symbol 'b')) (Symbol 'c')) (Symbol 'd')

regIter :: RegExpr
regIter = Iteration (Symbol 'a')


convert2 :: RegExpr -> Automaton
convert2 Epsilon = (1, "", [], 0, [0])
convert2 (Symbol a) = (2, [a], [(0, a, 1)], 0, [1])
convert2 (Iteration r) =
    let 
        (count, symbols, transitions, starts, ends) = convert2 r
        -- add new start state
        newStart = count
        -- add epsilon transitions from new start to old start
        newTransitions = (newStart, 'e', starts) : transitions
        -- add epsilon transitions from old ends to old start
        newTransitions' = [(end, 'e', starts) | end <- ends] ++ newTransitions
    in
        (count + 1, symbols, newTransitions', newStart, [newStart])
convert2 (Concat r1 r2) =
    let
        (count1, symbols1, transitions1, starts1, ends1) = convert2 r1
        (count2, symbols2, transitions2, starts2, ends2) = convert2 r2
        -- add node count to all transitions of r2
        newTransitions = [(node1 + count1, symbol, node2 + count1) | (node1, symbol, node2) <- transitions2]
        newStart = starts2 + count1
        newEnd = [end + count1 | end <- ends2]
        -- add epsilon transitions from old ends of r1 to new start of r2
        newTransitions' = [(end, 'e', starts1) | end <- newEnd] ++ newTransitions ++ transitions1
    in
        (count1 + count2, nub(symbols1 ++ symbols2), newTransitions', newStart, ends1)
convert2 (Alter r1 r2) =
    let
        (count1, symbols1, transitions1, starts1, ends1) = convert2 r1
        (count2, symbols2, transitions2, starts2, ends2) = convert2 r2
        -- add node count to all transitions of r2
        newTransitions = [(node1 + count1, symbol, node2 + count1) | (node1, symbol, node2) <- transitions2]
        newStart = starts2 + count1
        newEnd = [end + count1 | end <- ends2]
        -- add epsilon transitions from new start to old starts of r1 and r2
        newStart' = count1 + count2
        newTransitions' = (newStart', 'e', newStart) : (newStart', 'e', starts1) : newTransitions ++ transitions1
        newEnd' = ends1 ++ newEnd
    in
        (count1 + count2 + 1, nub(symbols1 ++ symbols2), newTransitions', newStart', newEnd')