import Data.List
import Control.Monad -- for forM_

isThirteen :: Integer -> IO ()
isThirteen x = case x of
        13 -> putStrLn "hello"
        _ -> putStrLn "nope"

hasName :: String -> String -> Bool
hasName s name = name `isInfixOf` s

isImportant :: String -> [String] -> Bool
isImportant s = any (hasName s)

main :: IO ()
main = do
        let var = 13
        isThirteen var

        isThirteen (var + 1)

        if "Name1" `isInfixOf` "< Name1> testing"
                then putStrLn "yes"
                else putStrLn "no"

        forM_ ["Name1", "Name2"] $ \s -> do
                if s `isInfixOf` "< Name1> testing"
                then putStrLn "yup"
                else putStrLn "nope"

        if isImportant "< Name1> testing" ["Name1", "Name2"]
        then putStrLn "wow"
        else putStrLn "mom"
