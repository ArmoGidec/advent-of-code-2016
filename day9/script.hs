import Data.Function ((&))
import System.Environment
import Data.Maybe
import Text.Regex

reg :: Regex
reg = mkRegex "\\([0-9]+x[0-9]+\\)"

parse :: String -> Maybe [Int]
parse text = matchRegex (mkRegex "([0-9]+)x([0-9]+)") text
    & fmap (map (\x -> read x :: Int))

decompress :: String -> String
decompress text = 
    case matchRegexAll reg text of
        Just (b, r, a, _) -> 
            let [q, w] = fromJust $ parse r
            in b ++ concat (replicate w $ take q a) ++ decompress (drop q a)
        Nothing -> text

compute1 :: String -> Int
compute1 input = length $ decompress input

repl :: Int -> String -> Int
repl n text = n * str where
    str = case matchRegexAll reg text of
        Just (_, r, a, _) -> 
            let [q, w] = fromJust $ parse r
            in repl w (take q a) + repl 1 (drop q a)
        Nothing -> length text

decompress2 :: String -> Int
decompress2 text = 
    case matchRegexAll reg text of
        Just (b, r, a, _) -> 
            let [q, w] = fromJust $ parse r
            in length b + repl w (take q a) + decompress2 (drop q a)
        Nothing -> length text

compute2 :: String -> Int
compute2 = decompress2
    
main :: IO()
main = do
    input <- getText
    let result1 = compute1 input;
        result2 = compute2 input
    print (result1, result2)

getText :: IO String
getText = do
    args <- getArgs
    let path = head args
    readFile path