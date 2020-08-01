{- 
    author: Wilber Bermeo [https://www.instagram.com/typewil] 
-}

module Controller
    (
        dispatch,
        play,
        resolve
    ) where

import System.IO
import Data.List
import Control.Monad
import Data.Maybe

import Definitions
    (
        World(..),
        Jelly(..),
        Area(..),
        Table,
        Volume,
        Point,
        toChar
    )

data Movement = Upward | Downward | Leftward | Rightward deriving Show

instance Eq Movement where
    Upward == Upward = True
    Downward == Downward = True
    Leftward == Leftward = True
    Rightward == Rightward = True
    _ == _ = False
    
---------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------

-- map of functions
dispatch :: [(String, World -> IO())]
dispatch =  [
            ("play",play),
            ("resolve",resolve)
            ]


toMovement :: Char -> Maybe Movement
toMovement c 
    | c == 'w' || c == 'W' || c == 'i' || c == 'I' = Just Upward
    | c == 's' || c == 'S' || c == 'k' || c == 'K' = Just Downward
    | c == 'a' || c == 'A' || c == 'j' || c == 'J' = Just Leftward
    | c == 'd' || c == 'D' || c == 'l' || c == 'L' = Just Rightward
    | otherwise = Nothing


-- there are just 4 type of movements
moveJelly :: Jelly -> Movement -> Jelly
moveJelly (Jelly((a,b),(x,y,z))) movement
    | movement == Upward = Jelly((a,b-z),(x,z,y))  
    | movement == Downward = Jelly((a,b+y),(x,z,y))
    | movement == Leftward = Jelly((a-z,b),(z,y,x))
    | otherwise = Jelly((a+x,b),(z,y,x))


whereIsJelly :: Jelly -> [Point]
whereIsJelly (Jelly((a,b),(x,y,_))) = [ (ai,bi) | ai <- [a..a'], bi <- [b..b'] ]
    where
        (a',b') = (a+x-1,b+y-1)

        
reachedGaol :: Jelly -> Table -> Bool
reachedGaol jelly tbl = jellyFits points tbl
    where
        points = whereIsJelly jelly


-- not implemented yet
jellyFits :: [Point] -> Table -> Bool
jellyFits points tbl = False


suckedDown :: Jelly -> Table -> Bool
suckedDown jelly@(Jelly((a,b),(x,y,_))) tbl = outOfTable || suckedDownWithInTable jelly tbl
    where
        outOfTable = a < 0 || b < 0 || (a+x) > length (head tbl) || (b+y) > length tbl


suckedDownWithInTable :: Jelly -> Table -> Bool
suckedDownWithInTable jelly@(Jelly((a,b),volume)) tbl = brokenIce points tbl volume || holeIgnored points tbl 
    where
        points = whereIsJelly jelly


-- if there is a ice in the table in any point of the collection,
-- the ice breaks if Jelly is lying with its major pressure possible
brokenIce :: [Point] -> Table -> Volume -> Bool
brokenIce points tbl (x,y,z) = thereIsIce points tbl && x*y <= y*z && x*y <= x*z 


thereIsIce :: [Point] -> Table -> Bool
thereIsIce points table = appearsThisArea points table Ice 


holeIgnored :: [Point] -> Table -> Bool
holeIgnored points table = appearsThisArea points table Hole


appearsThisArea :: [Point] -> Table -> Area -> Bool
appearsThisArea [] _ _ = False
appearsThisArea ((a,b):xs) tbl area = tbl !! b !! a == area || appearsThisArea xs tbl area


-- given the layout of the output I put on the layout Jelly's representation
generateOutput :: [String] -> Char -> [(Int,Int)] -> String
generateOutput canva _ [] = unlines canva
generateOutput canva jelly ((a,b):xs) = generateOutput canva' jelly xs
    where
        head = take a $ canva !! b
        tail = drop (a+1) $ canva !! b
        line = head ++ [jelly] ++ tail
        canva' = take b canva ++ [line] ++ drop (b+1) canva 


printWorld :: World -> IO()
printWorld (World(jelly,table)) = putStr $ generateOutput canva 'B' points 
    where
        canva = [ map toChar array | array <- table ]
        points = whereIsJelly jelly


play :: World -> IO()
play world@(World(jelly,table)) = do
                printWorld world
                line <- getLine
                if line /= "*"
                then do
                    let movement = toMovement $ head line
                    if isJust movement
                    then do
                        let jelly' = moveJelly jelly (fromJust movement)
                        if reachedGaol $ jelly' table
                        then
                            putStrLn "Has pasado el nivel, ¡Felicidades!"
                        else do
                            if suckedDown jelly' table -- aspirado
                            then
                                putStrLn "Se acabó, has caido al vacio"
                            else
                                play $ World(jelly', table)
                    else do
                        putStrLn "Movimiento impossible!"
                        play world
                else
                    putStrLn "UNA LASTIMA QUE NO QUIERAS SEGUIR JUGANDO... D':"


resolve :: World -> IO()
resolve world = do
                c <- getChar
                if c /= '*'
                then do
                    putChar c
                    resolve world
                else return ()
