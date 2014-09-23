module PrettyPrinter (
	ndfsm_to_fsm_string
) where

import State (Token(..), State(..), TransitionFunction(..), Transition(..), TransitionMap)
import qualified NDFSM (NDFSM(..))
import NDFSM (NDFSM, NDState, exploreTransitions, flatten, tabulate)
import FSM (mapTransitions)
import qualified Data.Set as Set (elems)
import Data.Bimap (Bimap, elems, (!>))
import Data.Map as Map (keys, (!))

--Returns a string representation of the generated FSM
ndfsm_to_fsm_string :: NDFSM -> String
ndfsm_to_fsm_string ndfsm = unlines $ map (printState transitionMap correspondence) states
	where
	nd_transitions = exploreTransitions ndfsm
	d_transitions = flatten nd_transitions
	-- Relationship from ND states to D states
	correspondence = tabulate nd_transitions 
	states = elems correspondence
	transitionMap = mapTransitions d_transitions

printState :: TransitionMap -> Bimap NDState State -> State -> String
printState tmap corr state = labelOf state ++ " : " 
								++ printDesc corr state ++ "\n"
								++ (unlines $ printTrans state tmap)
							

printDesc :: Bimap NDState State -> State -> String
printDesc corr state = concatMap ((++" ") . labelOf) $ Set.elems (corr !> state)

printTrans :: State -> TransitionMap -> [String]
printTrans state tmap = map ("  "++) [(show key) ++ " -> " ++ (labelOf (tmap' ! key)) | key <- keys tmap']
	where
	tmap' = tmap ! state