import Data.List.Split

parse :: String -> [String]
parse = splitOn "\n"



compute1 :: String -> String
compute1 input = do
	rooms = 
		input
		. parse
		. map parseRoom

compute2 :: String -> String
compute2 input = ""

main :: IO()
main = print (result1, result2) where
	input = getInput "input.txt"
	result1 = compute1
	result2 = compute2

getInput :: String -> String
getInput path = text <- readFile path