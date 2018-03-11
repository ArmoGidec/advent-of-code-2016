import Data.Monoid
import Data.List.Split
import Text.Regex.Posix
import System.Environment

type Room = (String, String, Int)

parse :: String -> Room
parse line = (name, checksum, id) where
    params = splitOn "-" line;
    name = mconcat $ init params
    checksum = last params =~ "([a-z]+)" :: String
    id = read (last params =~ "([0-9]+)" :: String) :: Int

resultate (name, checksum, id) = id

compute1 input = result where
    rooms = map parse (lines input)
    result = map resultate rooms

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