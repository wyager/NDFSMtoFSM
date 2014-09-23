module State (
	Token(..),
	State(..),
	TransitionFunction(..),
	Transition(..),
	TransitionMap
) where

import Data.Map (Map)

data Token = Epsilon | Token Char deriving (Eq, Ord, Show)

data State = State {labelOf :: String} deriving (Eq, Ord, Show)

type TransitionFunction = State -> Token -> State

type Transition = (State, Token, State)

type TransitionMap = Map State (Map Token State)