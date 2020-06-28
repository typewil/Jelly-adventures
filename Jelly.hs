{- 
    author: Wilber Bermeo [https://www.instagram.com/typewil] 
-}

import System.Environment
import System.IO
import Data.List

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
        error "WRONG NUMBER OF ARGUMENTS"
    else do
        program <- getProgName
        putStrLn $ program ++ " running..."

        content <- readFile path

        let (Just engine) = lookup (head action) dispatch
            (Just world) = createWorld content
        
        engine world        