import Data.List.Split
import System.Environment

type Position = (Int, Int)

keyboard1 :: [[Int]]
keyboard1 = [[1..3],[4..6],[7..9]]

keyboard2 :: [String]
keyboard2 = ["00100","02340","56789","0ABC0","00D00"]

parse :: String -> [String]
parse = splitOn "\n"

takeStep :: Char -> Position -> Position
takeStep step position = case step of
    'U' -> up position
    'D' -> down position
    'L' -> left position
    'R' -> right position

up :: Position -> Position
up (x, y) = (x, max 0 (y - 1))

down :: Position -> Position
down (x, y) = (x, min 2 (y + 1))

left :: Position -> Position
left (x, y) = (max 0 (x - 1), y)

right :: Position -> Position
right (x, y) = (min 2 (x + 1), y)

go :: String -> Position -> Position
go [] position = position
go (_:other) position = foldl (flip takeStep) position other

resultate :: Show a =>[String] -> Position -> String -> [[a]] -> String
resultate [] _ result _ = result
resultate (instruction:other) position result keyboard = 
    let (x, y) = go instruction position
        newResult = result ++ show (keyboard !! y !! x)
    in resultate other (x, y) newResult keyboard

compute1 :: String -> String
compute1 input = result where
    instructions = parse input
    result = resultate instructions (1, 1) "" keyboard1

compute2 :: String -> String
compute2 input = "result2"
    
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