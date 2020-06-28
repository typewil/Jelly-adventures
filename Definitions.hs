{- 
    author: Wilber Bermeo [https://www.instagram.com/typewil] 
-}

module Definitions
    (
        Jelly(..),
        Table(..),
        World(..),
        createWorld
    ) where

type Point2D = (Int,Int)
type Dimen3D = (Int,Int,Int)

data Box = Groud | Hole | Wall | Meta
data Jelly = Jelly (Point2D,Dimen3D)
data Table = Table [[Box]] 
data World = World (Jelly,Table)

---------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------

createWorld :: String -> Maybe World
createWorld content = do
                    let
                        divContent = lines content
                        table = initTable $ drop 2 divContent
                        jelly = initJelly divContent
                        match = matchTest table jelly

                    if match
                    then
                        Just $ World (jelly,table)
                    else
                        Nothing


initTable :: [String] -> Table
initTable table = Table [[]]
    where
        dimX = length $ head table
        dimY = length table


initJelly :: [String] -> Jelly
initJelly _ = Jelly ((0,0),(1,1,1))


matchTest :: Table -> Jelly -> Bool
matchTest _ _ = True 
