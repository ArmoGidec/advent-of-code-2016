import qualified Data.Set as Set
import Data.List
import Data.Function (on, (&))
import Data.List.Split
import Text.Regex.Posix
import System.Environment

type Room = (String, String, Int)

task :: String -> [(Char, Int)]
task list = 
    let set = Set.fromList list
    in map instanceIn (Set.toList set) where
        instanceIn x = (x, length $ filter (==x) list)

getMax :: Eq a => [(a, Int)] -> [(a,Int)]
getMax = sortBy (flip compare `on` snd)

parse :: String -> Room
parse line = (name, checksum, roomId) where
    params = splitOn "-" line
    name = mconcat $ init params
    checksum = last params =~ "([a-z]+)" :: String
    roomId = read (last params =~ "([0-9]+)" :: String) :: Int

resultate :: Room -> Bool 
resultate (name, checksum, _) =
    let s = task name
    in checksum == fst (unzip $ take 5 $ getMax s)

compute1 :: String -> Int
compute1 input = input
    & lines
    & map parse
    & filter resultate
    & foldl (\acc (_, _, roomId) -> acc + roomId) 0

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