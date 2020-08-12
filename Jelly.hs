{- 
    author: Wilber Bermeo [https://www.instagram.com/typewil] 
-}

module Jelly
    (
    ) where

import System.Environment
import System.IO
import Data.List
import Data.Maybe

import Solver
    (
        resolve
    )

import Controller
    (
        play
    )

import Definitions 
    (
        World(..),
        createWorld
    )   

---------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------

-- map of functions
dispatch :: [(String, World -> IO())]
dispatch =  [
            ("play",play),
            ("resolve",resolve)
            ]


main = do
    -- rescuing arguments
    -- 1. map name
    -- 2. play/resolve
    args@(path:action) <- getArgs

    if length args < 2
    then
        error "NÚMERO DE ARGUMENTOS ERRONEO"
    else do
        program <- getProgName
        putStrLn $ program ++ " esta cargando..."

        content <- readFile path

        let (Just engine) = lookup (head action) dispatch
            world = createWorld content
        
        if isJust world
        then do
            putStrLn "Juego cargado correctamente!"
            engine $ fromJust world
        else 
            putStrLn "Hubo un problema al crear el juego"
