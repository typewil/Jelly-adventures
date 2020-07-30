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

data Area   = Meta | Ground | Ice | Hole
data Jelly  = Jelly (Point2D,Dimen3D)
data Table  = Table [[Area]] 
data World  = World (Jelly,Table)

---------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------

createWorld :: String -> Maybe World
createWorld content = do
                    let
                        rows = lines content
                        table = initTable $ drop 1 rows
                        jelly = initJelly rows
                        match = matchTest table jelly

                    if match
                    then
                        Just $ World (jelly,table)
                    else
                        Nothing


initTable :: [String] -> Table
initTable table = Table $ initTable' table


-- creates game table with a part of the content of the game file
initTable' :: [String] -> [[Area]]
initTable' [] = []
initTable' (x:xs) = (parseStringToArrayArea x) : initTable' xs


parseStringToArrayArea :: String -> [Area]
parseStringToArrayArea array = [ toArea x | x <- array ]


toArea :: Char -> Area
toArea c
    | c == '0' = Meta
    | c == '1' = Ground
    | c == '2' = Ice
    | otherwise = Hole


initJelly :: [String] -> Jelly
initJelly (l:xl) = Jelly ((a,b),(x,y,z))
    where
        z = read l :: Int -- the first line in the file is the height of the jelly
        ((a,b),(x,y)) = startingArea xl


-- the first tuple is where the are stars and the second tuple is the dimentions of this area
startingArea :: [String] -> ((Int,Int),(Int,Int))
startingArea lines = ((xi,yi),(x,y))
    where 
        ((xi,yi),(xf,yf)) = nearAndFarPointStartArea lines
        x = xf - xi + 1
        y = yf - yf + 1


nearAndFarPointStartArea :: [String] -> ((Int,Int),(Int,Int))
nearAndFarPointStartArea lines@(l:_) = 
    where
        points = [(a,b) | b <- [0..(length lines - 1)], a <- [0..(length l - 1)]]


matchTest :: Table -> Jelly -> Bool
matchTest _ _ = True 
