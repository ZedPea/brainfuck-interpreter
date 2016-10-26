import qualified Data.Sequence as S (Seq, replicate, length, replicate,
                                        adjust, index, update)
import Data.Sequence ((><))
import Data.Char (chr, ord)
import Data.Int (Int8)
import Control.Monad (void)
import System.IO (isEOF, hFlush, stdout)
import System.Environment (getArgs)
import Utilities

main :: IO ()
main = do
    args <- getArgs
    if any (`elem` helpFlags) args then help else do
    maybeFile <- getFile args
    case maybeFile of
        Nothing -> interpreter
        Just file -> do
        source <- readFile file
        let s = State byteArray 0 source source
        parse' s

data State = State {
    memory :: S.Seq Int8,
    pointer :: Int,
    totalInput :: String,
    nonConsumedInput :: String
}

--our memory array, it is 8 bit, and starts with 30,000 cells
byteArray :: S.Seq Int8
byteArray = S.replicate 30000 0

{-
we don't need a state returned in main, so this wrapper function is used
to make main a tad cleaner
-}
parse' :: State -> IO ()
parse' s = void $ parse s

{-
parse input while it still remains. if it's a non brainfuck character we have
to ignore it. the update function takes care of incrementing the
nonConsumedInput, so the few functions which don't call go, need to call that
themeselves. We are careful to leave our nonConsumedInput on the [ or ]
characters while starting and stopping loops, as it is incremented after this 
function is called, putting the pointer at the character following the bracket
-}
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

{-
if we have the space, just increment the pointer. Otherwise, add 1000 more
cells to memory
-}
incrementPointer :: State -> State
incrementPointer s@(State m p _ _)
    | pointer s < S.length m = s { pointer = p + 1 }
    | otherwise = s { pointer = p + 1, memory = newmem }
    where newmem = m >< S.replicate 1000 0

{-
we could potentially expand the array to the left, but it would be illogical
for the instruction pointer to go below zero, so I don't.
-}
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
    end <- isEOF
    if end then return s else do
        c <- fromIntegral . ord <$> getChar
        let newMem = S.update p c m
        return $ s { memory = newMem }

{-
we use tail here so the first [ gets chopped off. Otherwise, the counter will
be incorrectly incremented
-}
startLoop :: State -> State
startLoop s@(State m p _ n)
    | S.index m p == 0 = s { nonConsumedInput = nextBracket (tail n) 0 }
    | otherwise = s

{-
to handle nested brackets while looping, each time we encounter a [, we 
increment our count by 1. If we meet a ] and the counter is not 0, then we 
have more layers of nesting to leave. If we get to the end of the input
then the programmer must have made a mistake so we throw an error.
-}
nextBracket :: String -> Int -> String
nextBracket [] _ = error "Mismatched [ error"
nextBracket z@(x:xs) count
    | x == ']' && count == 0 = z
    | x == ']' = nextBracket xs (pred count)
    | x == '[' = nextBracket xs (succ count)
    | otherwise = nextBracket xs count

endLoop :: State -> State
endLoop s@(State m p _ _)
    | S.index m p /= 0 = previousBracket s
    | otherwise = s

{-
we reverse the input before the nonconsumedinput as we need to search backwards
and this makes it a lot easier.
-}
previousBracket :: State -> State
previousBracket s = s { nonConsumedInput = find search n 0 }
    where search = reverse $ take (length t - length n) t
          find [] _ _ = error "Mismatched ] error"
          find (x:xs) acc count
            | x == '[' && count == 0 = x : acc
            | x == '[' = find xs (x:acc) (pred count)
            | x == ']' = find xs (x:acc) (succ count)
            | otherwise = find xs (x:acc) count
          t = totalInput s
          n = nonConsumedInput s

--remove one character from the nonconsumedinput - it has just been parsed.
update :: State -> State
update s@(State _ _ _ n)
    | null n = s
    | otherwise = s { nonConsumedInput = tail n }

interpreter :: IO ()
interpreter = do
    putStrLn ">>> Brainfuck interpreter (Ctrl+D to exit) <<<"
    interpreter'
    
interpreter' :: IO ()
interpreter' = do
    putStr "bf: "
    hFlush stdout
    end <- isEOF
    if end then putStrLn "" else do
    input <- getLine
    parse' $ State byteArray 0 input input
    interpreter'

help :: IO ()
help = putStr =<< readFile "help.txt"
