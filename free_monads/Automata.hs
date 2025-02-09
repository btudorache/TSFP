module Automata where

import Control.Monad

type Q = Int
type Σ = Char

data DFA q σ = DFA
    { initial    :: q
    , transition :: q -> σ -> q
    , isFinal    :: q -> Bool
    }

-- https://en.wikipedia.org/wiki/Deterministic_finite_automaton#Example
evenZeros :: DFA Q Σ
evenZeros = DFA 1 transition (== 1)
  where
    transition q σ = case (q, σ) of
        (1, '0') -> 2
        (1, '1') -> 1
        (2, '0') -> 1
        (2, '1') -> 2

{-
    *** TODO 1 ***

    Implement runDFA.
-}
runDFA :: DFA q σ -> [σ] -> q
runDFA (DFA init trans _) transitions = foldl trans init transitions

acceptsDFA :: DFA q σ -> [σ] -> Bool
acceptsDFA dfa = isFinal dfa . runDFA dfa

data NFA q σ = NFA
    { initial'    :: q
    , transition' :: q -> σ -> [q]
    , isFinal'    :: q -> Bool
    }

-- https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton#Example
-- p = 1, q = 2
endsWithOne :: NFA Q Σ
endsWithOne = NFA 1 transition (== 2)
  where
    transition q σ = case (q, σ) of
        (1, '0') -> [1]
        (1, '1') -> [1, 2]
        _        -> []  -- missing transitions

{-
    *** TODO 2 ***

    Implement runNFA, through a minimal modification of the definition of runDFA
-}
runNFA :: NFA q σ -> [σ] -> [q]
runNFA (NFA init trans _) transitions = foldl step [init] transitions
  where
    step states symbol = concatMap (\q -> trans q symbol) states

acceptsNFA :: NFA q σ -> [σ] -> Bool
acceptsNFA nfa = any (isFinal' nfa) . runNFA nfa