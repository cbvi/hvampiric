import Data.List
--import System.Exit
import Control.Monad (when)
import System.IO
import System.IO.Error
import Control.Exception (catch)

data LogFile = LogFile  { logName :: String
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
        if z > a
                then Just (drop (a + 1) $ take (z + a) s)
                else Nothing

isImportant :: String -> [String] -> Bool
isImportant s l = case parseName s of
        Just n -> any (hasName n) l
        Nothing -> False

processLine :: String -> IO ()
processLine s = when (isImportant s ["Name2", "Name1"]) $ putStrLn s

processLines :: Handle -> IO ()
processLines h = hGetLine h >>= processLine >> processLines h

eofHandler :: IOError -> IO ()
eofHandler e
        | isEOFError e = return ()
        | otherwise = ioError e

main :: IO ()
main = do
        let logFiles = [LogFile   { logName = "log2"
                        , logPath = "log2.log"
                        , logImportant = ["Name1", "Name2"]
                        }]

        res <- tryIOError $ openFile "test.txt" ReadMode
        case res of
                Left _ -> return ()
                Right h -> do
                        processLines h `catch` eofHandler
                        i <- hTell h
                        print i
                        hClose h

        mapM_ (putStrLn . logName) logFiles
