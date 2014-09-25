module ExampleNDFSMs (
	abba_ndfsm
) where

import State (Token(..), State(..))
import NDFSM (NDFSM(..))
import Data.Set (fromList, singleton, empty)


	
-- An NDFSM to match (a|b)*abba(a|b)*
abba_ndfsm :: NDFSM
abba_ndfsm = 
	NDFSM {
		states = fromList [State "initial", State "a0", State "b0", State "b1", State "a1"],
		state0 = State "initial",
		accepting = singleton (State "a1"),
		transitionFunction = tf,
		alphabet = fromList [Token 'a', Token 'b']
	}
	where
		tf (State "initial") (Token 'a') = fromList [State "initial", State "a0"]
		tf (State "initial") (Token 'b') = singleton (State "initial")
		tf (State "a0") (Token 'b') = singleton (State "b0")
		tf (State "b0") (Token 'b') = singleton (State "b1")
		tf (State "b1") (Token 'a') = singleton (State "a1")
		tf (State "a1") _ = singleton (State "a1")
		tf _ _ = empty

