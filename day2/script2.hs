import Data.List.Split
import System.Environment

type Position = (Int, Int)

keyboard :: [String]
keyboard = ["00100","02340","56789","0ABC0","00D00"]

parse :: String -> [String]
parse = splitOn "\n"

takeStep :: Char -> Position -> Position
takeStep step position = case step of
    'U' -> up position
    'D' -> down position
    'L' -> left position
    'R' -> right position

up :: Position -> Position
up (x, y) = (x, y - 1)

down :: Position -> Position
down (x, y) = (x, y + 1)

left :: Position -> Position
left (x, y) = (x - 1, y)

right :: Position -> Position
right (x, y) = (x + 1, y)

distance :: Position -> Int
distance (x, y) = abs (x - 2) + abs (y - 2) 

go :: String -> Position -> Position
go [] position = position
go (step:other) oldPosition = go other position where
    newPosition = takeStep step oldPosition
    position 
        | distance newPosition < 3 = newPosition
        | otherwise = oldPosition

resultate :: [String] -> Position -> String -> String
resultate [] _ result = result
resultate (instruction:other) position result = 
    let (x, y) = go instruction position
        newResult = result ++ show (keyboard !! y !! x)
    in resultate other (x, y) newResult

compute1 :: String -> String
compute1 input = result where
    instructions = parse input
    result = resultate instructions (1, 1) "" 
    
main :: IO()
main = do
    input <- getText
    let result1 = compute1 input
    print result1

getText :: IO String
getText = do
    args <- getArgs
    let path = head args
    readFile path