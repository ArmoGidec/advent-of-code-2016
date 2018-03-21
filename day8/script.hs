import Data.Function ((&))
import Data.List (transpose)
import Data.List.Split (splitOn)
import System.Environment
import Text.Regex.Posix

initScreen :: [String]
initScreen = replicate 6 (replicate 50 '.')

parse :: [String] -> [Int]
parse (str:_) = map read $ splitOn "x" str :: [Int]

rect :: [String] -> [Int] -> [String]
rect screen [_,0] = screen
rect (line:screen) [x,y] = 
    let str = replicate x '#' ++ drop x line
    in str : rect screen [x, y - 1]

change xy n screen = 
    let str = screen !! xy;
        len = length str - n
        newStr = drop len str ++ take len str
    in take xy screen ++ [newStr] ++ drop (xy + 1) screen

rotate :: [String] -> [String] -> [String]
rotate screen (direction:xy:_:n:_) = 
    case direction of
        "row" -> f screen
        "column" -> transpose screen
            & f
            & transpose
        _ -> fail "Wrong Input"
    where f = change (read (xy =~ "([0-9]+)" :: String) :: Int) (read n :: Int)

execute :: [String] -> String -> [String]
execute screen line =
    let command = words line;
        f = case head command of
            "rect" -> rect screen (parse $ tail command)
            "rotate" -> rotate screen (tail command)
            _ -> fail "Wrong Input"
    in f

calc :: Int -> String -> Int
calc acc line = acc + length (filter (== '#') line)

compute1 :: String -> String
compute1 input = 
    lines input
    & foldl execute initScreen
    & foldl calc 0 
    & show

compute2 :: String -> [String]
compute2 input = 
    lines input
    & foldl execute initScreen
    
main :: IO()
main = do
    input <- getText
    let result1 = compute1 input;
        result2 = compute2 input
    print result1
    writeFile "result.txt" (unlines result2)

getText :: IO String
getText = do
    args <- getArgs
    let path = head args
    readFile path