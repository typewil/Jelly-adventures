import System.Environment
import System.IO
import Data.List

---------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------

-- GAME'S DATA
data Jelly = Jelly Int Int Int deriving Show
data Table = Table Int Int deriving Show
data World = World (Table, Jelly) deriving Show

---------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------

initWorld :: String -> Maybe World
initWorld content = do
                    let
                        divContent = lines content
                        table = initTable $ drop 2 divContent
                        jelly = initJelly divContent
                        match = matchTest table jelly

                    if match
                    then
                        Just $ World (table,jelly)
                    else
                        Nothing


-- I asume that the necessary data to create de table is in the file
initTable :: [String] -> Table
initTable table = Table dimX dimY
    where
        dimX = length $ head table
        dimY = length table


-- Again, I asume that the necessary data to create de table is in the file
initJelly :: [String] -> Jelly
initJelly _ = Jelly 1 1 2 


matchTest :: Table -> Jelly -> Bool
matchTest _ _ = True 


-- map of functions
dispatch :: [(String, World -> IO())]
dispatch =  [("play",play),
            ("resolve",resolve)
            ]


play :: World -> IO()
play world = putStrLn "GAME MODE: Human playing"


resolve :: World -> IO()
resolve world = putStrLn "GAME MODE: hehe the most intelligent of us is playing, that is, me."


main = do
    -- rescuing arguments
    -- 1. map name
    -- 2. play/resolve
    args@(action:path) <- getArgs

    if length args < 2
    then
        error "WRONG NUMBER OF ARGUMENTS"
    else do
        program <- getProgName
        putStrLn $ program ++ " running..."

        content <- readFile $ head path

        let (Just engine) = lookup action dispatch  -- returns Nothing if does not find the element in the map
            (Just world) = initWorld content
        
        engine world        