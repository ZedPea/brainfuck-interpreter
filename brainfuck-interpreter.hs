import qualified Data.Sequence as S
import Data.Char (chr, ord)
import Data.Int

main :: IO ()
main = do
    source <- readFile "source.bf"
    let s = State byteArray 0 source source
    _ <- parse s
    return ()

data State = State {
    memory :: S.Seq Int8,
    pointer :: Int,
    totalInput :: String,
    nonConsumedInput :: String
}

--our memory array
byteArray :: S.Seq Int8
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
printValue (State m p _ _) = putChar $ chr $ fromIntegral $ S.index m p

getCharacter :: State -> IO State
getCharacter s@(State m p _ _) = do
    c <- fromIntegral . ord <$> getChar
    let newMem = S.update p c m
    return $ s { memory = newMem }

startLoop :: State -> State
startLoop s@(State m p _ n)
    | S.index m p == 0 = s { nonConsumedInput = nextBracket (tail n) 0 }
    | otherwise = s

nextBracket :: String -> Int -> String
nextBracket [] _ = error "Mismatched ["
nextBracket z@(x:xs) count
    | x == ']' && count == 0 = z
    | x == ']' = nextBracket xs (pred count)
    | x == '[' = nextBracket xs (succ count)
    | otherwise = nextBracket xs count

endLoop :: State -> State
endLoop s@(State m p _ _)
    | S.index m p /= 0 = previousBracket s
    | otherwise = s

previousBracket :: State -> State
previousBracket s@(State _ _ t n) = s { nonConsumedInput = find search n 0 }
    where search = reverse $ take (length t - length n) t
          find [] _ _ = error "Mismatched ]"
          find (x:xs) acc count
            | x == '[' && count == 0 = x : acc
            | x == '[' = find xs (x:acc) (pred count)
            | x == ']' = find xs (x:acc) (succ count)
            | otherwise = find xs (x:acc) count

update :: State -> State
update s@(State _ _ _ n)
    | null n = s
    | otherwise = s { nonConsumedInput = tail n }
