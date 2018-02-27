import Data.List.Split
import Data.Maybe

data Instruction = Instruction { rotate :: Char, stepsCount :: Int} deriving Show
type Position = (Int, Int)

initPosition :: Position
initPosition = (0, 0)

parse :: String -> [String]
parse = splitOn ", "

parseInstruction :: String -> Instruction
parseInstruction instruction = Instruction {
    rotate = head instruction,
    stepsCount = read $ tail instruction
}

turnLeft :: Char -> Char
turnLeft direction = 
    case direction of
        'S' -> 'E'
        'E' -> 'N'
        'N' -> 'W'
        'W' -> 'S'

turnRigth :: Char -> Char
turnRigth direction = 
    case direction of
        'S' -> 'W'
        'W' -> 'N'
        'N' -> 'E'
        'E' -> 'S'

toTurn :: Char -> Char -> Char
toTurn rotation direction
    | rotation == 'R' = turnRigth direction
    | otherwise = turnLeft direction

toStep :: Position -> Char -> Int -> Maybe Position -> [Position] -> (Position, Maybe Position, [Position])
toStep (x, y) _ 0 twice history = ((x, y), twice, history)
toStep (x, y) direction steps twice history = case direction of
    'S' -> toStep (x, y + 1) 'S' (steps - 1) newTwice newHistory
    'W' -> toStep (x + 1, y) 'W' (steps - 1) newTwice newHistory
    'N' -> toStep (x, y - 1) 'N' (steps - 1) newTwice newHistory
    'E' -> toStep (x - 1, y) 'E' (steps - 1) newTwice newHistory
    where 
        newTwice = 
            if isNothing twice
            then isTwice (x, y) history
            else twice
        newHistory = (x, y) : history

isTwice :: Position -> [Position] -> Maybe Position
isTwice position history
    | position `elem` history = Just position
    | otherwise = Nothing

go :: [Instruction] -> Position -> Char -> [Position] -> Maybe Position -> (Position, Position )
go [] position _ _ twice = (position, fromJust twice)
go instructions position direction history twice = let
    instruction = head instructions
    steps = stepsCount instruction
    newDirection = toTurn (rotate instruction) direction
    (newPosition, newTwice, newHistory) = toStep position newDirection steps twice history
    in go (tail instructions) newPosition newDirection newHistory newTwice

distance :: Position -> Int
distance (x, y) = abs x + abs y

main :: IO()
main = print (result1, result2) where
    instructionsText = parse text
    instructions = map parseInstruction instructionsText
    result = go instructions initPosition 'S' [] Nothing
    result1 = distance $ fst result
    result2 = distance $ snd result
    -- result2 = distance (115,-123)

text :: String
text = "R5, R4, R2, L3, R1, R1, L4, L5, R3, L1, L1, R4, L2, R1, R4, R4, L2, L2, R4, L4, R1, R3, L3, L1, L2, R1, R5, L5, L1, L1, R3, R5, L1, R4, L5, R5, R1, L185, R4, L1, R51, R3, L2, R78, R1, L4, R188, R1, L5, R5, R2, R3, L5, R3, R4, L1, R2, R2, L4, L4, L5, R5, R4, L4, R2, L5, R2, L1, L4, R4, L4, R2, L3, L4, R2, L3, R3, R2, L2, L3, R4, R3, R1, L4, L2, L5, R4, R4, L1, R1, L5, L1, R3, R1, L2, R1, R1, R3, L4, L1, L3, R2, R4, R2, L2, R1, L5, R3, L3, R3, L1, R4, L3, L3, R4, L2, L1, L3, R2, R3, L2, L1, R4, L3, L5, L2, L4, R1, L4, L4, R3, R5, L4, L1, L1, R4, L2, R5, R1, R1, R2, R1, R5, L1, L3, L5, R2"