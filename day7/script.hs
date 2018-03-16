import Data.Function ((&))
import Data.List.Split
import System.Environment

everyEven (x:y:xs) = x : everyEven xs
everyEven (x:_) = [x]
everyEven _ = []

toGroup groups = (evenGroups, oddGroups) where
    evenGroups = everyEven groups
    oddGroups = everyEven (tail groups)

parse = splitOneOf "[]"

isContainAbba group = 
    case group of
        a:b:c:d:other -> (a==d && b==c && a/=b) || isContainAbba (tail group)
        _ -> False

isSupportTls (insideGroup, outsideGroup) = any isContainAbba insideGroup && not (any isContainAbba outsideGroup)

compute1 :: String -> Int
compute1 input = 
    lines input
    & map parse
    & map toGroup
    & filter isSupportTls
    & length

findAbs chars =
    case chars of
        a:b:c:rest ->
            if (a /= b) && (a == c) then
                (a, b) : findAbs (tail chars)
            else findAbs(tail chars)
        _ -> [] 

isSupportSsl (insideGroup, outsideGroup) = 
    let abs = outsideGroup & concatMap findAbs;
        bas = insideGroup & concatMap findAbs & map (\(x,y) -> (y,x))
    in any (`elem` bas) abs

compute2 :: String -> Int
compute2 input = 
    lines input
    & map parse
    & map toGroup
    & filter isSupportSsl
    & length
    
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