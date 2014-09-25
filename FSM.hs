module FSM (
	FSM(..),
	mapTransitions,
	mapToFunc,
	accepts
) where

import State (Token(..), State(..), TransitionFunction(..), Transition(..), TransitionMap)
import Data.Set as Set (Set, elems, member)
import Data.Map as Map (Map, (!), empty, insert, singleton, member)

data FSM = FSM {states :: Set State, state0 :: State, accepting :: Set State, 
					transitionFunction :: TransitionFunction, alphabet :: Set Token}

-- Turns a set describing some transitions into a map of transitions
mapTransitions :: Set Transition -> TransitionMap
mapTransitions set = foldr addTransition Map.empty $ elems set
	where
	addTransition :: Transition -> Map State (Map Token State) -> Map State (Map Token State)
	addTransition (a, t, b) map = map'
		where
		map' = if a `Map.member` map
			then Map.insert a (Map.insert t b (map ! a)) map
			else Map.insert a (Map.singleton t b) map

-- Turns a 2-nested map into a 2-arity function
mapToFunc :: (Ord a, Ord b) => Map a (Map b c) -> (a -> b -> c)
mapToFunc m a b = (m ! a) ! b

accepts :: FSM -> [Token] -> Bool
fsm `accepts` string = accepts' fsm (state0 fsm) string
accepts' :: FSM -> State -> [Token] -> Bool
accepts' fsm state [] = state `Set.member` (accepting fsm)
accepts' fsm state (t:tt) = accepts' fsm state' tt
	where state' = transitionFunction fsm state t