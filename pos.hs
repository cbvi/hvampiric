import Data.List

main = do
        let s = "XX:XX < test> testing"
        -- let a = drop 5 (take 10 s)

        let start = case (elemIndex '<' s) of
                Just v -> v
                Nothing -> -1

        let stop = case (elemIndex '>' (drop start s)) of
                Just v -> v + start
                Nothing -> -1

        print start
        print stop

        let name = drop (start + 1) (take stop s)

        if isInfixOf "test" name
                then putStrLn "works"
                else putStrLn "nope"

        putStrLn name
