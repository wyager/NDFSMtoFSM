import State (Token(..), State(..), TransitionFunction(..), Transition(..), TransitionMap)
import qualified NDFSM (NDFSM(..))
import NDFSM (NDFSM, NDState, exploreTransitions, flatten, tabulate, acceptsState, eps)
import qualified FSM (FSM(..))
import FSM (FSM, mapTransitions, mapToFunc, accepts)
import qualified Data.Set as Set (fromList, singleton, empty)
import Data.Bimap (elems, keys, (!))
import PrettyPrinter (ndfsm_to_fsm_string)
import Control.Applicative ((<$>))
import ExampleNDFSMs (abba_ndfsm)

-- Transforms an NDFSM into an FSM
ndfsm_to_fsm :: NDFSM -> FSM
ndfsm_to_fsm ndfsm = FSM.FSM {FSM.states = Set.fromList $ elems correspondence,
							FSM.state0 = correspondence ! (eps ndfsm $ NDFSM.state0 ndfsm),
							FSM.accepting = Set.fromList $ 
											[correspondence ! ndstate | 
											ndstate <- keys correspondence, 
											ndfsm `acceptsState` ndstate], 
							FSM.transitionFunction = mapToFunc transitionMap,
							FSM.alphabet = NDFSM.alphabet ndfsm }
					where
					nd_transitions = exploreTransitions ndfsm
					d_transitions = flatten nd_transitions
					-- Bimap correspondence between ND states and D states
					correspondence = tabulate nd_transitions 
					transitionMap = mapTransitions d_transitions

	



my_ndfsm = abba_ndfsm
my_fsm = ndfsm_to_fsm my_ndfsm
strings = ["abab", "babb", "abb", "babba", "abbaabba", "abbabbbb"]

main = do
		putStrLn "generated FSM:"
		putStrLn $ ndfsm_to_fsm_string my_ndfsm
		putStrLn $ "matching " ++ show strings
		let matches = (my_fsm `accepts`) <$> map Token <$> strings
		putStrLn $ "matches: " ++ show matches