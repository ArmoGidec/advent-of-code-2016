import Data.List
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

compute1 :: String -> String
compute1 input = result where
    trianglesParams = map parse (lines input)
    triangles = filter isTriangle trianglesParams
    result = show $ length triangles

transform :: [[Int]] -> [[Int]] -> [[Int]]
transform [] params = params
transform (fline:sline:tline:other) params = transform other (params ++ transpose [fline, sline, tline])

compute2 :: String -> String
compute2 input = result where
    trianglesParams = map parse (lines input)
    newTrianglesParams = transform trianglesParams []
    triangles = filter isTriangle newTrianglesParams
    result = show $ length triangles

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