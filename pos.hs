import Data.List
import System.Exit
import Control.Monad (when, unless)
import Control.Exception
import System.IO
import System.IO.Error
import Data.Maybe (fromMaybe)

getOffsets :: String -> Maybe (Int, Int)
getOffsets s = do
        i <- elemIndex '<' s
        d <- elemIndex '>' (drop i s)
        return (i, i+d)

hasName :: String -> String -> Bool
hasName s name = name `isInfixOf` s

isImportant :: String -> [String] -> Bool
isImportant s = any $ hasName s

parsedName :: String -> (Int, Int) -> String
parsedName s (a, b) = drop (a + 1) $ take b s

parseName :: String -> Maybe (String)
parseName s = do
        o <- getOffsets s
        return $ parsedName s o

isSane :: (Int, Int) -> Bool
isSane (-1, _) = False
isSane (_, -1) = False
isSane (a, b) = b > a

newmain :: String -> IO ()
newmain s = do
        res <- tryIOError $ openFile "test.txt" ReadMode
        case res of
                Left e -> putStrLn "oops"
                Right h -> do
                        hClose h

        -- let off = fromMaybe (-1, -1) (getOffsets s)

        -- unless (isSane off) exitFailure

        -- let name = parsedName s off
        let name = fromMaybe "" $ parseName s

        putStrLn name

        when (isImportant name ["test", "testl"])
                $ putStrLn s

main :: IO ()
main = do
        let s = "XX:XX < testl> testing"
        newmain s
