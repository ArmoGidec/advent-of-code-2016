import Data.List.Split

type Position = (Int, Int)
initPosition = (1, 1)

keyboard = [[1..3],[4..6],[7..9]]

takeStep step position = case step of
	'U' -> up position
	'D' -> down position
	'L' -> left position
	'R' -> right position

up (x, y) = (x, max 0 (y - 1))

down (x, y) = (x, min 2 (y + 1))

left (x, y) = (max 0 (x - 1), y)

right (x, y) = (min 2 (x + 1), y) 

go [] position = position
go instruction position = do
	step = head instruction
	newPosition = takeStep step position
	newInstruction = tail instruction
	go newInstruction newPosition

compute1 [] _ result = result
compute1 instructions position result = 
	newPosition = go (head instructions) position
	newResult = result + keyboard !! y !! x
	compute1 (tail instructions) newPosition newResult

parse :: String -> [String]
parse = splitOn "\n"

main :: IO()
main = do
	text <- readFile "input.txt"
	instructions = parse text
	result1 = compute1 instructions initPosition ""
	print (result1, "")