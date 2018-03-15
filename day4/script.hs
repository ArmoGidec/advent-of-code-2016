import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Data.Function (on, (&))
import Data.List.Split
import Text.Regex.Posix
import System.Environment

type Room = (String, String, Int)

alphabet :: String
alphabet = "abcdefghijklmnopqrstuvwxyz"

task :: String -> [(Char, Int)]
task list = 
    let set = Set.fromList list
    in map instanceIn (Set.toList set) where
        instanceIn x = (x, length $ filter (==x) list)

getMax :: Eq a => [(a, Int)] -> [(a,Int)]
getMax = sortBy (flip compare `on` snd)

parse1 :: String -> Room
parse1 line = (name, checksum, roomId) where
    params = splitOn "-" line
    name = mconcat $ init params
    checksum = last params =~ "([a-z]+)" :: String
    roomId = read (last params =~ "([0-9]+)" :: String) :: Int

resultate :: Room -> Bool 
resultate (name, checksum, _) =
    let s = task name
    in checksum == fst (unzip $ take 5 $ getMax s)

compute1 :: String -> Int
compute1 input = lines input
    & map parse1
    & filter resultate
    & foldl (\acc (_, _, roomId) -> acc + roomId) 0

parse2 :: String -> ([String], Int)
parse2 line = (name, spaces) where
    params = splitOn "-" line
    name = init params
    spaces = read (last params =~ "([0-9]+)" :: String) :: Int

decrypt_char :: Int -> Char -> Char
decrypt_char len char = 
    (elemIndex char alphabet
    & fromJust
    & (+) len) `mod` 26
    & (!!) alphabet

decrypt :: Int -> String -> String
decrypt len = 
    map (decrypt_char len)

resultate2 :: ([String], Int) -> (String, Int)
resultate2 (str, len) = 
    let sentence = map (decrypt (len `mod` 26)) str
            & unwords
    in (sentence, len)

findNorthPole :: (String, Int) -> Bool
findNorthPole (sentence, _) = words sentence
    & elem "northpole"

compute2 :: String -> Int
compute2 input = lines input
    & map parse2
    & map resultate2
    & filter findNorthPole
    & head
    & snd

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