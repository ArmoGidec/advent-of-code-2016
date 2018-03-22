import Data.Function ((&))
import System.Environment
import Data.Maybe
import Text.Regex
import Text.Regex.Posix

reg1 :: Regex
reg1 = mkRegex "\\([0-9]+x[0-9]+\\)"

parse text = matchRegex (mkRegex "([0-9]+)x([0-9]+)") text
    & fmap (map (\x -> read x :: Int))

decompress :: String -> String
decompress text = 
    case matchRegexAll reg1 text of
        Just (b, r, a, _) -> 
            let [q, w] = fromJust $ parse r
            in b ++ concat (replicate w $ take q a) ++ decompress (drop q a)
        Nothing -> text

compute1 :: String -> Int
compute1 input = length $ decompress input


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