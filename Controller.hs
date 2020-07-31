{- 
    @autor: [typewil](https://www.instagram.com/typewil/)
-}

module Controller
    (
        dispatch,
        play,
        resolve
    ) where

import System.IO
import Data.List

import Definitions
    (
        World(..)
    )

---------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------

-- map of functions
dispatch :: [(String, World -> IO())]
dispatch =  [
            ("play",play),
            ("resolve",resolve)
            ]

play :: World -> IO()
play world = putStrLn "GAME MODE: Human playing"


resolve :: World -> IO()
resolve world = putStrLn "GAME MODE: hehe the most intelligent of us is playing, that is, me."




