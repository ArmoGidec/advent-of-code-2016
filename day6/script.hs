import Data.List
import qualified Data.Set as Set
import System.Environment
import Data.Function (on)

task :: String -> [(Char, Int)]
task list = 
    let set = Set.fromList list
    in map instanceIn (Set.toList set) where
        instanceIn x = (x, length $ filter (==x) list)

getMax :: Eq a => [(a, Int)] -> [(a,Int)]
getMax = sortBy (flip compare `on` snd)
  
compute1 :: String -> String
compute1 input = result where
    xs = transpose $ lines input
    result = map ((fst . head . getMax) . task) xs

compute2 :: String -> String
compute2 input = result where
    xs = transpose $ lines input
    result = map ((fst . last . getMax) . task) xs
    
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