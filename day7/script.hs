import Data.List.Split
import System.Environment

parse line = concatMap (splitOn "]") (splitOn "[" line)

-- compute1 :: String -> String
compute1 = parse . head . lines 

compute2 :: String -> String
compute2 input = result where
    result = "result2"
    
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