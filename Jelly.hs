{- 
    author: Wilber Bermeo [https://www.instagram.com/typewil] 
-}

import System.Environment
import System.IO
import Data.List
import Data.Maybe

import Controller
    (
        dispatch,
        play,
        resolve
    )

import Definitions 
    (
        createWorld
    )   

---------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------

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
