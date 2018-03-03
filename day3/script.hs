import Data.List.Split
import System.Environment

parse params = map (\x -> read x :: Int) (words params)

isTriangle params
    | a + b > c && a + c > b && b + c > a = True
    | otherwise = False
    where
        a = head params
        b = params !! 1
        c = params !! 2

compute1 input = result where
    trianglesParams = map parse (lines input)
    triangles = filter isTriangle trianglesParams
    result = length triangles

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
    readFile $ head args