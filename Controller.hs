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
        Jelly(..)
    )

data Movement = Forward | Backward | Leftward | Rightward deriving Show

instance Eq Movement where
    Forward == Forward = True
    Backward == Backward = True
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
    | c == 'w' || c == 'W' || c == 'i' || c == 'I' = Just Forward
    | c == 's' || c == 'S' || c == 'k' || c == 'K' = Just Backward
    | c == 'a' || c == 'A' || c == 'j' || c == 'J' = Just Leftward
    | c == 'd' || c == 'D' || c == 'l' || c == 'L' = Just Rightward
    | otherwise = Nothing


-- there just 4 type of movements
moveJelly :: Jelly -> Movement -> Jelly
moveJelly jelly movement
    | movement == Forward = moveForward jelly  
    | movement == Backward = moveBackward jelly
    | movement == Leftward = moveLeftward jelly
    | otherwise = moveRightward jelly


moveForward :: Jelly -> Jelly
moveForward jelly@(Jelly(p1,(x,y,z))) = Jelly(p1,(x,z,y))


moveBackward :: Jelly -> Jelly
moveBackward jelly@(Jelly(p1,(x,y,z))) = Jelly(p1,(x,z,y))


moveLeftward :: Jelly -> Jelly
moveLeftward jelly@(Jelly(p1,(x,y,z))) = Jelly(p1,(z,y,x))


moveRightward :: Jelly -> Jelly
moveRightward jelly@(Jelly(p1,(x,y,z))) = Jelly(p1,(z,y,x))


play :: World -> IO()
play world@(World(jelly,table)) = do
                print world
                line <- getLine
                if line /= "*"
                then do
                    let movement = toMovement $ head line
                    if isJust movement
                    then do -- here I should update the table maybe and check if there was a solution found
                        let supplanter = moveJelly jelly (fromJust movement) 
                        play $ World(supplanter, table)
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



