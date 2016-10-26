import qualified Data.Sequence as S
import Data.Char (chr, ord)

main :: IO ()
main = do
    source <- readFile "source.bf"
    let s = State byteArray 0 source source
    _ <- parse s
    return ()

data State = State {
    memory :: S.Seq Int,
    pointer :: Int,
    totalInput :: String,
    nonConsumedInput :: String
}

--our memory array
byteArray :: S.Seq Int
byteArray = S.replicate 10000 0

parse :: State -> IO State
parse s
    | null $ nonConsumedInput s = return s
    | otherwise =
    case next of
        '>' -> go incrementPointer
        '<' -> go decrementPointer
        '+' -> go incrementValue
        '-' -> go decrementValue
        '.' -> printValue s >> parse (update s)
        ',' -> do
            newState <- getCharacter s
            parse $ update newState
        '[' -> go startLoop
        ']' -> go endLoop
        _ -> parse $ update s
    where next = head $ nonConsumedInput s
          go f = let updatedState = f s
                 in parse (update updatedState)

incrementPointer :: State -> State
incrementPointer s@(State _ p _ _)
    | pointer s < 9999 = s { pointer = p + 1 }
    | otherwise = error "Out of memory error"

decrementPointer :: State -> State
decrementPointer s@(State _ p _ _)
    | pointer s > 0 = s { pointer = p - 1 }
    | otherwise = error "Pointer lowered below zero error"

incrementValue :: State -> State
incrementValue s@(State m _ _ _) = let newMem = S.adjust succ (pointer s) m
                                   in s { memory = newMem }

decrementValue :: State -> State
decrementValue s@(State m _ _ _) = let newMem = S.adjust pred (pointer s) m
                                   in s { memory = newMem }

printValue :: State -> IO ()
printValue (State m p _ _) = putChar $ chr $ S.index m p

getCharacter :: State -> IO State
getCharacter s@(State m p _ _) = do
    c <- ord <$> getChar
    let newMem = S.update c p m
    return $ s { memory = newMem }

-- if value at pointer == 0, go to closing bracket
-- else keep parsing
startLoop :: State -> State
startLoop s@(State m p _ n)
    | S.index m p == 0 = s { nonConsumedInput = nextBracket }
    | otherwise = s
    where nextBracket = dropWhile (/= ']') n

endLoop :: State -> State
endLoop s@(State m p _ _)
    | S.index m p /= 0 = previousBracket s
    | otherwise = s

previousBracket :: State -> State
previousBracket s@(State _ _ t n) = s { nonConsumedInput = find search n }
    where search = reverse $ take (length t - length n) t
          find [] _ = error "ya dun goofed"
          find (x:xs) acc
            | x == '[' = x : acc
            | otherwise = find xs (x : acc)

update :: State -> State
update s@(State _ _ _ n)
    | null n = s
    | otherwise = s { nonConsumedInput = tail n }
