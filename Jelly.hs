import System.Environment
import System.IO
import Data.List

-- map of functions
dispatch :: [(String, String -> IO())]
dispatch =  [("play",play),
            ("resolve",resolve)
            ]


initWorld :: String -> Maybe String
initWorld pathWorld = Just pathWorld


play :: String -> IO()
play _ = putStrLn "GAME MODE: Human playing"


resolve :: String -> IO()
resolve _ = putStrLn "GAME MODE: hehe the most intelligent of us is playing, that is, me."


main = do
    -- rescuing arguments
    -- 1. map name
    -- 2. play/resolve
    args@(action:xs) <- getArgs

    if length args < 2
    then
        error "WRONG NUMBER OF ARGUMENTS"
    else do
        program <- getProgName
        putStrLn $ program ++ " running..."

        let (Just engine) = lookup action dispatch -- returns Nothing if does not find the element in the map
            (Just world) = initWorld (head xs)

        engine world