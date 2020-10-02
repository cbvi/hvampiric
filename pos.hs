import Data.List
import System.Exit
import Control.Monad (when, unless)
import Control.Exception
import System.IO
import System.IO.Error

start :: String -> Int
start s  = case elemIndex '<' s of
        Just v  -> v
        Nothing -> -1

stop :: String -> Int -> Int
stop s o = case elemIndex '>' $ drop o s of
        Just v  -> v + o
        Nothing -> -1

hasName :: String -> String -> Bool
hasName s name = name `isInfixOf` s

isImportant :: String -> [String] -> Bool
isImportant s = any $ hasName s

parsedName :: String -> Int -> Int -> String
parsedName s a b = drop (a + 1) $ take b s

isSane :: Int -> Int -> Bool
isSane (-1) _ = False
isSane _ (-1) = False
isSane a b = b > a

newmain :: String -> IO ()
newmain s = do
        let a = start s
        let b = stop s a

        res <- tryIOError $ openFile "test.txt" ReadMode
        case res of
                Left e -> putStrLn "oops"
                Right h -> do
                        hClose h

        unless (isSane a b) exitFailure

        let name = parsedName s a b

        when (isImportant name ["test", "testl"])
                $ putStrLn s

main :: IO ()
main = do
        let s = "XX:XX < test> testing"
        newmain s
