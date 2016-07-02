module Transition (
      Transition(..)
    , (<.>)
    , (<|>)
    ) where

import Prelude hiding ((<*>), Left, Right)

----------------
-- Data types --
----------------

data Transition =
      Concat Transition Transition
    | Or Transition Transition
    | Star Transition
    | Up
    | Down
    | Left
    | Right
    | A
    | B
    | Select
    | Start
    | Epsilon

instance Show Transition where
    show (Concat t1 t2) = "(" ++ show t1 ++ show t2 ++ ")"
    show (Or t1 t2)     = "(" ++ show t1 ++ "|" ++ show t2 ++ ")"
    show (Star t)       = "(" ++ show t ++ ")" ++ "*"
    show Up             = "▲"
    show Down           = "▼"
    show Left           = "◀"
    show Right          = "▶"
    show A              = "A"
    show B              = "B"
    show Select         = "Select"
    show Start          = "Start"

-----------------------------------------------------
-- Transition types as convenient inline operators --
-----------------------------------------------------

infixl 1 <.>
(<.>) :: Transition -> Transition -> Transition
(<.>) = Concat

infixl 0 <|>
(<|>) :: Transition -> Transition -> Transition
(<|>) = Or
