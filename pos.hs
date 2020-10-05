import Data.List
--import System.Exit
import Control.Monad (when)
import System.IO
import System.IO.Error
import Control.Exception (catch)
import Debug.Trace

data LogFile = LogFile
                { logName :: String
                , logPath :: String
                , logImportant :: [String]
                }

hasName :: String -> String -> Bool
hasName s name = name `isInfixOf` s

--isImportant :: String -> [String] -> Bool
--isImportant s = any $ hasName s

parseName :: String -> Maybe String
parseName s = do
        a <- elemIndex '<' s
        z <- elemIndex '>' $ drop a s
        if (a + z) > a
                then Just (drop (a + 1) $ take (z + a) s)
                else trace s $ Nothing

isImportant :: String -> [String] -> Bool
isImportant s l = case parseName s of
        Just n -> any (hasName n) l
        Nothing -> False

processLine :: String -> [String] -> IO ()
processLine s l = when (isImportant s l) $ putStrLn s

-- processLines h = hGetLine h >>= processLine >> processLines h
processLines :: Handle -> [String] -> IO ()
processLines h n = do
        l <- hGetLine h
        processLine l n
        processLines h n

eofHandler :: IOError -> IO ()
eofHandler e
        | isEOFError e = return ()
        | otherwise = ioError e

logHeader :: LogFile -> String
logHeader l = "=== " ++ logName l ++ " ==="

processLog :: LogFile -> IO ()
processLog l = do
        f <- openFile (logPath l) ReadMode
        putStrLn . logHeader $ l
        processLines f (logImportant l) `catch` eofHandler
        i <- hTell f
        print i
        hClose f
        putStrLn "\n\n"

main :: IO ()
main = do
        let logFiles = [
                        LogFile 
                        { logName = "log2"
                        , logPath = "log2.log"
                        , logImportant = ["Name1", "Name2"]
                        },
                        LogFile
                        { logName = "log1"
                        , logPath = "log1.log"
                        , logImportant = ["Name3"]
                        }
                        ]

        mapM_ processLog logFiles
