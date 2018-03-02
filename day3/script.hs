import Data.List.Split

parse :: String -> [String]
parse = splitOn "\n"

split :: String -> [Int]
split = map (\x -> read x) $ splitOn " "

isTriangle :: [Int] -> Bool
isTriangle [a, b, c] 
	| a + b > c && a + c > b && d + c > a = Truw
	| otherwise = False

compute1 :: String -> String
compute1 input = do
	trianglesParam = parse input . map split
	triangles = filter (\triangle -> isTriangle triangle) trianglesParam
	return $ show (length triangles) 

main :: IO()
main = print (result1, result2) where
	input = getInput "input.txt"
	result1 = compute1 input
	result2 = ""

getInput :: String -> String
getInput path = text <- readFile path