module NDFSM (
	NDFSM(..),
	NDState,
	exploreTransitions,
	flatten,
	tabulate,
	acceptsState,
	eps
) where


import State (Token(..), State(..), TransitionFunction(..), Transition(..))
import Data.Set as Set (Set, fromList, singleton, empty, insert, unions, union, elems, showTree, intersection, (\\))
import qualified Data.Set as Set (map, null) 
import Data.Bimap as Bimap (Bimap, fromList, (!))

type NDState = Set State

type NDTransitionFunction = State -> Token -> NDState

type NDTransition = (NDState, Token, NDState)

data NDFSM = NDFSM {states :: Set State, state0 :: State, accepting :: Set State, 
					transitionFunction :: NDTransitionFunction, alphabet :: Set Token}


eps :: NDFSM -> State -> Set State
eps fsm state = eps' fsm state Set.empty
eps' :: NDFSM -> State -> Set State -> Set State
eps' fsm state visited = state `insert` reachable 
	where
	reachable = unions [eps' fsm state' visited' | state' <- elems unvisited]
	visited' = state `insert` visited
	unvisited = (transitionFunction fsm) state Epsilon \\ visited 

apply :: NDFSM -> NDState -> Token -> NDState
apply fsm state0 token = state'_eps
	where
	state' = unions [transitionFunction fsm state token | state <- elems state0]
	state'_eps = unions [eps fsm state | state <- elems state']


exploreTransitions :: NDFSM -> Set NDTransition
exploreTransitions fsm = exploreTransitions' fsm (singleton $ state0 fsm) Set.empty
exploreTransitions' :: NDFSM -> NDState -> Set (NDState, Token) -> Set NDTransition
exploreTransitions' fsm state visited = Set.fromList transitions `union` transitions'
	where
	possible = Set.fromList [(state, token) | token <- elems (alphabet fsm)]
	unvisited = possible \\ visited
	visited' = visited `union` unvisited
	transitions = [(state, token, apply fsm state token) 
									| (state, token) <- elems unvisited]
	transitions' = unions [exploreTransitions' fsm state visited' | (_,_,state) <- transitions]

tabulate :: Set NDTransition -> Bimap NDState State
tabulate set = table
	where
	states = Set.map (\(a,_,_) -> a) set `union` Set.map (\(_,_,b) -> b) set
	table = Bimap.fromList $ zip (elems states) $ map (State . show) [0,1..]

flatten :: Set NDTransition -> Set Transition
flatten set = Set.map (\(a, t, b) -> (table ! a, t, table ! b)) set
	where table = tabulate set


acceptsState :: NDFSM -> NDState -> Bool
ndfsm `acceptsState` state = not . Set.null $ (intersection (accepting ndfsm) state)
