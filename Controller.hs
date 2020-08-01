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


reachedGaol :: World -> Bool
reachedGaol _ = False


fellIntoVoid :: World -> Bool
fellIntoVoid _ = False


play :: World -> IO()
play world@(World(jelly,table)) = do
                printWorld world
                line <- getLine
                if line /= "*"
                then do
                    let movement = toMovement $ head line
                    if isJust movement
                    then do -- here I should update the table maybe and check if there was a solution found
                        let jelly' = moveJelly jelly (fromJust movement)
                        if reachedGaol $ World(jelly',table)
                        then
                            putStrLn "Has pasado el nivel, ¡Felicidades!"
                        else do
                            if fellIntoVoid $ World(jelly',table)
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


-- given the layout of the output I put on the layout Jelly
generateOutput :: [String] -> [(Int,Int)] -> String
generateOutput canva [] = unlines canva
generateOutput canva ((a,b):xs) = generateOutput canva' xs
    where
        head = take a $ canva !! b
        tail = drop (a+1) $ canva !! b
        line = head ++ ['5'] ++ tail
        canva' = take b canva ++ [line] ++ drop (b+1) canva 


printWorld :: World -> IO()
printWorld (World(Jelly((a,b),(x,y,_)),table)) = putStrLn $ generateOutput canva points 
    where
        canva = [ map toChar array | array <- table ]
        (a',b') = (a+x-1,b+y-1)
        points = [ (ai,bi) | ai <- [a..a'], bi <- [b..b'] ]