import Data.List
--import System.Exit
import Control.Monad (when)
import System.IO
import System.IO.Error
import Control.Exception (catch)

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
        let s = "XX:XX < testl> testing"
        res <- tryIOError $ openFile "test.txt" ReadMode
        case res of
                Left _ -> putStrLn "oops"
                Right h -> do
                        catch (processLines h) eofHandler
                        i <- hTell h
                        print i
                        hClose h

        when (isImportant s ["test", "testl"])
                $ putStrLn s
