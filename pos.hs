import Data.List
--import System.Exit
import Control.Monad (when)
import System.IO
import System.IO.Error
import Control.Exception (catch)
import Debug.Trace
import Data.Binary
import Data.Int
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString)

data LogFile = LogFile
   { logName :: String
   , logPath :: String
   , logImportant :: [String]
   , logSkip :: Int64
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

processLog :: LogFile -> IO (Int64)
processLog l = do
    f <- openFile (logPath l) ReadMode
    hSeek f AbsoluteSeek (fromIntegral (logSkip l))
    putStrLn . logHeader $ l
    processLines f (logImportant l) `catch` eofHandler
    i <- hTell f
    hClose f
    putStrLn "\n"
    return (fromIntegral i)

decodeOff :: [Int64] -> ByteString -> [Int64]
decodeOff xs bs = case decodeOrFail bs of
    Left (_, _, _) -> xs
    Right (b, _, v) -> decodeOff (xs ++ [v]) b

decodeOffs :: ByteString -> [Int64]
decodeOffs bs = decodeOff [] bs

entHandler :: IOError -> IO (ByteString)
entHandler e
    | isDoesNotExistError e = return b
    | otherwise = ioError e
    where a = [z, z]
          b = B.concat $ map encode a
          z = (0 :: Int64)

main :: IO ()
main = do
    cur <- B.readFile "offsets.dat" `catch` entHandler
    let coffs = decodeOffs cur

    let logFiles = [ LogFile { logName = "log2"
                             , logPath = "log2.log"
                             , logImportant = ["Name1", "Name2"]
                             , logSkip = (coffs !! 0)
                             }
                   , LogFile { logName = "log1"
                             , logPath = "log1.log"
                             , logImportant = ["Name3"]
                             , logSkip = (coffs !! 1)
                             }
                   ]

    offs <- mapM processLog logFiles
    let bin = B.concat $ map encode offs

    B.writeFile "offsets.dat" bin
