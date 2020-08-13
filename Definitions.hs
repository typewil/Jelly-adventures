{- 
    @autor: [Cantact me](https://www.instagram.com/wilberquito/)
-}

module Definitions
    (
        Jelly(..),
        World(..),
        Area(..),
        Route(..),
        States(..),
        Movement(..),
        Table,
        Point,
        Volume,
        createWorld,
        toChar,
        nearAndFarPoints
    ) where

type Point = (Int,Int)
type Volume = (Int,Int,Int)
type Table  = [[Area]]
-- movements to achive the goal 
type Route = [Movement]
type States = [Jelly]

data Area = Goal | Ground | Ice | Hole deriving Show
data Jelly = Jelly (Point,Volume) deriving Show
data World = World (Jelly,Table)  deriving Show
data Movement = Upward | Downward | Leftward | Rightward deriving Show

instance Eq Movement where
    Upward == Upward = True
    Downward == Downward = True
    Leftward == Leftward = True
    Rightward == Rightward = True
    _ == _ = False

instance Eq Area where
    Goal == Goal = True
    Ground == Ground = True
    Ice == Ice = True
    Hole == Hole = True
    _ == _ = False

instance Eq Jelly where
    (Jelly(p1,v1)) == (Jelly(p2,v2)) = p1 == p2 && v1 == v2

---------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------

createWorld :: String -> Maybe World
createWorld content = do
                    let
                        rows = lines content
                        table = initTable $ drop 1 rows
                        jelly@(Jelly(_,volume))= initJelly rows
                        
                    if couldFitInGoal table volume
                    then
                        Just $ World (jelly,table)
                    else
                        Nothing


-- creates game table with a part of the content of the game file
initTable :: [String] -> Table
initTable [] = []
initTable (x:xs) = (map toArea x) : initTable xs


toArea :: Char -> Area
toArea c
    | c == '0' = Hole
    | c == '1' = Goal
    | c == '2' = Ice
    | otherwise = Ground


toChar :: Area -> Char
toChar a
    | a == Hole = '0'
    | a == Goal = '1'
    | a == Ice = '2'
    | otherwise = '3'


initJelly :: [String] -> Jelly
initJelly (l:xl) = Jelly ((a,b),(x,y,z))
    where
        z = read l :: Int -- the first line in the file is the height of the jelly
        ((a,b),(x,y)) = pointAndStartingArea xl


-- the first tuple is where the are stars and the second tuple is the dimentions of this area
pointAndStartingArea :: [String] -> ((Int,Int),(Int,Int))
pointAndStartingArea lines = ((xi,yi),(x,y))
    where 
        ((xi,yi),(xf,yf)) = nearAndFarPoints lines 'B'
        x = xf - xi + 1
        y = yf - yi + 1


nearAndFarPoints :: Eq a => [[a]] -> a -> ((Int,Int),(Int,Int))
nearAndFarPoints rows@(r:_) target = (p1,p2)
    where
        points = [(x,y) | y <- [0..(length rows - 1)], x <- [0..(length r - 1)]]
        areaPoints = [ p | p <- points, appearsAtThisPoint p target rows ]
        p1 = head areaPoints
        p2 = areaPoints !! (length areaPoints - 1)


-- 5 is where jelly is on the map
appearsAtThisPoint :: Eq a => (Int,Int) -> a -> [[a]] -> Bool
appearsAtThisPoint (x,y) target table = table !! y !! x == target


-- I check if a face of Jelly fits the goal
couldFitInGoal :: Table -> Volume -> Bool
couldFitInGoal table (x,y,z) = (xGaol >= x && yGoal >= y || xGaol >= y && yGoal >= x) || (xGaol >= y && yGoal >= z || xGaol >= z && yGoal >= y) || (xGaol >= x && yGoal >= z || xGaol >= z && yGoal >= x) 
    where
        (xGaol,yGoal) = dimentionsGoal table


dimentionsGoal :: Table -> (Int,Int)
dimentionsGoal table = (x,y)
    where
        ((xi,yi),(xf,yf)) = nearAndFarPoints table Goal
        x = xf - xi + 1
        y = yf - yi + 1