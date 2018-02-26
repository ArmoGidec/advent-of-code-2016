import Data.List.Split

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
    | rotation == 'L' = turnLeft direction

toStep :: Position -> Char -> Int -> Position
toStep (x, y) direction steps = case direction of
    'S' -> (x, y + steps)
    'W' -> (x + steps, y)
    'N' -> (x, y - steps)
    'E' -> (x - steps, y)

go :: [Instruction] -> Position -> Char -> [Position] -> (Position, [Position])
go [] position _ history = (position, history)
go instructions position direction history = let
    instruction = head instructions
    steps = stepsCount instruction
    newDirection = toTurn (rotate instruction) direction
    newPosition = toStep position newDirection steps
    newHistory = newPosition : history
    in go (tail instructions) newPosition newDirection newHistory

distance :: Position -> Int
distance (x, y) = abs x + abs y

main :: IO()
main = print (result1, result2) where
    instructionsText = parse text
    instructions = map parseInstruction instructionsText
    result = go instructions initPosition 'S' []
    result1 = distance $ fst result
    result2 = snd result

text :: String
text = "R5, R4, R2, L3, R1, R1, L4, L5, R3, L1, L1, R4, L2, R1, R4, R4, L2, L2, R4, L4, R1, R3, L3, L1, L2, R1, R5, L5, L1, L1, R3, R5, L1, R4, L5, R5, R1, L185, R4, L1, R51, R3, L2, R78, R1, L4, R188, R1, L5, R5, R2, R3, L5, R3, R4, L1, R2, R2, L4, L4, L5, R5, R4, L4, R2, L5, R2, L1, L4, R4, L4, R2, L3, L4, R2, L3, R3, R2, L2, L3, R4, R3, R1, L4, L2, L5, R4, R4, L1, R1, L5, L1, R3, R1, L2, R1, R1, R3, L4, L1, L3, R2, R4, R2, L2, R1, L5, R3, L3, R3, L1, R4, L3, L3, R4, L2, L1, L3, R2, R3, L2, L1, R4, L3, L5, L2, L4, R1, L4, L4, R3, R5, L4, L1, L1, R4, L2, R5, R1, R1, R2, R1, R5, L1, L3, L5, R2"