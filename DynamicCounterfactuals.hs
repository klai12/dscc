module DynamicCounterfactuals where

import Control.Monad.State
import Data.List ((\\), union)

-- primitive types
type Player = String
type World = String
type Prop = String
type Move = String

-- composite types
type Vector = [(Player, Move)]
type Strategy = World -> [Move]

data Model = Model { players :: [Player],
                     worlds :: [World],
                  -- at each world, what propositions are true
                     label_f :: World -> [Prop],
                  -- from each world, given move vector, what is next world
                     transition_f :: World -> Vector -> World,
                  -- for each player, what their strategy is
                     strategy_f :: Player -> World -> [Move] }

         -- propositional logic formulas
data Form = Pr Prop | Ng Form | Cnj Form Form | Dsj Form Form
          | Cond Form Form | Bicond Form Form
         -- top and bottom
          | Top | Bottom
         -- modal formulas
          | Diamond Form | Box Form
         -- ATL+I strategy term
          | Str Player Strategy Form

-- given a proposition p and model m, returns the set of worlds that satisfy p
reg :: Prop -> State Model [World]
reg p = gets $ \m -> [q | q <- (worlds m), p `elem` (label_f m) q]

-- given a world q and model m, returns a list of move vectors (i.e. Cartesian
-- product of moves) active at q
vectors :: World -> Model -> [Vector]
vectors q m = sequence [[(a, j) | j <- (strategy_f m) a q] | a <- (players m)]

-- given a set of worlds, returns the set of worlds for which at least one
-- world in the set is accessible
pre :: [World] -> State Model [World]
pre rho = gets $ \m -> [q | q <- (worlds m),
                            any (`elem` rho) [(transition_f m) q v
                                              | v <- vectors q m]]

-- revise function edits the set of active strategies
revise :: Player -> Strategy -> State Model ()
revise a strategy = do
    m <- get
    let new_strategy_f b q
            | a == b    = (strategy_f m) b q `union` strategy q
            | otherwise = (strategy_f m) b q
    put $ m { strategy_f = new_strategy_f }

-- model checker for formulas
-- given a formula and a model, returns the set of worlds that satisfy the
-- formula
check :: Form -> State Model [World]
-- for propositions, use reg to get the set of worlds
check (Pr prop) = reg prop
-- not, or should be straightforward
check (Ng phi) = do
    m <- get
    worlds_phi <- check phi
    return ((worlds m) \\ worlds_phi)
check (Dsj phi psi) = do
    worlds_phi <- check phi
    worlds_psi <- check psi
    return (worlds_phi `union` worlds_psi)
-- for and, if, iff, use equivalences
check (Cnj phi psi) = check (Ng (Dsj (Ng phi) (Ng psi)))
check (Cond phi psi) = check (Dsj (Ng phi) psi)
check (Bicond phi psi) = check (Cnj (Cond phi psi) (Cond psi phi))
-- top is true everywhere, bottom is true nowhere
check Top = gets worlds
check Bottom = return []
-- for diamond, use pre to get the set of worlds where in at least one
-- accessible world, phi is true
check (Diamond phi) = do
    worlds_phi <- check phi
    pre worlds_phi
-- for box, use equivalence
check (Box phi) = check (Ng (Diamond (Ng phi)))
-- for Str, revise the model
check (Str a strategy phi) = do
    revise a strategy
    check phi
