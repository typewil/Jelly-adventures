module Solver
    (

    ) where

import System.IO
import Data.Maybe
import Data.List

import Definitions
    (
        World(..),
        Jelly(..),
        Table
    )

import Controller
    (
        Movement(..),
        moveJelly,
        suckedDown,
        reachedGaol
    )

-- movements to achive the goal 
type Route = [Movement]
type States = [Jelly]


-- if returs an empty list, then in this state Jelly has no possible movement
forwardMovements :: World -> [Movement]
forwardMovements world = [ mvnt | mvnt <- movements, properMovement mvnt world]
    where 
        movements = [Upward, Downward, Leftward, Rightward]


properMovement :: Movement -> World -> Bool
properMovement mvnt (World(jelly,table)) = not $ suckedDown (moveJelly jelly mvnt) table


-- here we pass the map and a list with the first state of Jelly
-- and returns a solution or Nothing. If the response is Nothing, there is no solution
breadthFirst :: Table -> States -> [(Jelly,Route)] -> Maybe Route
breadthFirst table visited lvl =    if isJust solution
                                    then
                                        snd (fromJust solution)
                                    else
                                        if lvl == []
                                        then 
                                            Nothing
                                        else do
                                            let visited' = updateVisitedStates lvl visited
                                            breadthFirst table visited' (expand table visited lvl)

    where
        func = reachedGaol table
        solution = find func lvl -- returns the first element that satisfies the predicate


updateVisitedStates :: [(Jelly,Route)] -> States -> States
updateVisitedStates [] states = states
updateVisitedStates ((jelly,_):xs) states = jelly : updateVisitedStates xs


-- for each node in the current lvl I generate other nodes if it is possible
-- at the first beggining there is only one node, the root, it can generate at most 4 new nodes
expand :: Table -> States -> [(Jelly,Route)] -> [(Jelly,Route)]
expand _ _ [] = []
expand tbl visited ((jelly,route):xs) = newNodes ++ expand tbl visited xs 
    where
        -- it is needed to rescue only the movements that avoid to return to state already visited
        utilMovements = notCyclicalMovements jelly tbl visited
        newNodes = expandNode (jelly,route) utilMovements


-- given a node that represents the state of jelly in the game, I generate other nodes
-- with the movements that have been passed
expandNode :: (Jelly,Route) -> [Movement] -> [(Jelly,Route)]
expandNode _ [] = []
expandNode (jelly,route) (mvnt:xs) = (jelly', route') : expandNode (jelly,route) xs
    where
        route' = route ++ [mvnt]
        jelly' = moveJelly jelly


reachedGaol' :: Table -> (Jelly,Route) -> Bool
reachedGaol' table (state,_) = reachedGaol state table


-- avoiding cyclical movements
notCyclicalMovements :: Jelly -> Table -> States -> [Movement]
notCyclicalMovements jelly tbl states = utilMovements
    where
        possibleMovements = forwardMovements (World(jelly,tbl))
        -- ading to the collection only the movements that does not generate states in wich Jelly has been previously
        utilMovements = [ mvnt | mvnt <- possibleMovements, not ((moveJelly jelly mvnt) `elem` states) ]
