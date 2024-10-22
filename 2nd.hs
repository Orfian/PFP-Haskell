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


-- Convert a regular expression to DFA
convert2 :: RegExpr -> Automaton
convert2 r = convertNfaDfa (convertRegNfa r)

convertNfaDfa :: Automaton -> Automaton
convertNfaDfa (nStates, alphabet, transitions, start, acceptStates) = (nStates, alphabet, transitions, start, acceptStates)

convertRegNfa :: RegExpr -> Automaton
convertRegNfa Epsilon = (1, "", [], 0, [0])
convertRegNfa (Symbol a) = (2, [a], [(0, a, 1)], 0, [1])
convertRegNfa (Iteration r) =
    let 
        (count, symbols, transitions, starts, ends) = convertRegNfa r
        -- add new start state
        newStart = count
        -- add epsilon transitions from new start to old start
        newTransitions = (newStart, 'e', starts) : transitions
        -- add epsilon transitions from old ends to old start
        newTransitions' = [(end, 'e', starts) | end <- ends] ++ newTransitions
    in
        (count + 1, symbols, newTransitions', newStart, [newStart])
convertRegNfa (Concat r1 r2) =
    let
        (count1, symbols1, transitions1, starts1, ends1) = convertRegNfa r1
        (count2, symbols2, transitions2, starts2, ends2) = convertRegNfa r2
        -- add node count to all transitions of r2
        newTransitions = [(node1 + count1, symbol, node2 + count1) | (node1, symbol, node2) <- transitions2]
        newStart = starts2 + count1
        newEnd = [end + count1 | end <- ends2]
        -- add epsilon transitions from old ends of r1 to new start of r2
        newTransitions' = [(end, 'e', starts1) | end <- newEnd] ++ newTransitions ++ transitions1
    in
        (count1 + count2, nub(symbols1 ++ symbols2), newTransitions', newStart, ends1)
convertRegNfa (Alter r1 r2) =
    let
        (count1, symbols1, transitions1, starts1, ends1) = convertRegNfa r1
        (count2, symbols2, transitions2, starts2, ends2) = convertRegNfa r2
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