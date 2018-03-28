import Data.Function ((&))
import System.Environment

column :: [[String]]
column = [["E","PromG","PromM",""],["CobG","CurG","RutG","PlutG"], ["CobM","CurM","RutM","PlutM"]]

compute1 :: String -> String
compute1 input = result where
    result = "result1"

compute2 :: String -> String
compute2 input = result where
    result = "result2"
    
main :: IO()
main = do
    input <- getText
    let result1 = compute1 input;
        result2 = compute2 input
    print result1
    print result2

getText :: IO String
getText = do
    args <- getArgs
    let path = head args
    readFile path