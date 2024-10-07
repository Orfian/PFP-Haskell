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
