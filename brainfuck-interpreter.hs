{-# LANGUAGE TemplateHaskell #-}
import qualified Data.Sequence as S (Seq, replicate, length, replicate,
                                        adjust, index, update)
import Data.Sequence ((><))
import Data.Char (chr, ord)
import Data.Int (Int8)
import Control.Monad (void)
import System.IO (isEOF, hFlush, stdout)
import ParseArgs
import System.Console.CmdArgs (cmdArgs)
import System.Directory (doesFileExist)
import Control.Lens

data BFState = BFState {
    _memory :: S.Seq Int8,
    _pointer :: Int,
    _totalInput :: String,
    _nonConsumedInput :: String
}

makeLenses ''BFState

main :: IO ()
main = do
    args <- cmdArgs bf
    exists <- doesFileExist $ file args
    if exists then do
        source <- readFile $ file args
        parse' $ BFState byteArray 0 source source
    else interpreter

--our memory array, it is 8 bit, and starts with 30,000 cells
byteArray :: S.Seq Int8
byteArray = S.replicate 30000 0

{- we don't need a state returned in main, so this wrapper function is used
to make main a tad cleaner -}
parse' :: BFState -> IO ()
parse' = void . parse

{- parse input while it still remains. if it's a non brainfuck character we
have to ignore it. the update function takes care of incrementing the
nonConsumedInput, so the few functions which don't call go, need to call that
themeselves. We are careful to leave our nonConsumedInput on the [ or ]
characters while starting and stopping loops, as it is incremented after this 
function is called, putting the pointer at the character following the
bracket -}
parse :: BFState -> IO BFState
parse state
    | null $ state^.nonConsumedInput = return state
    | otherwise =
    case next of
        '>' -> go incrementPointer
        '<' -> go decrementPointer
        '+' -> go incrementValue
        '-' -> go decrementValue
        '.' -> printValue state >> parse (update state)
        ',' -> do
            newState <- getCharacter state
            parse $ update newState
        '[' -> go startLoop
        ']' -> go endLoop
        _ -> parse $ update state
    where next = head $ state^.nonConsumedInput
          go f = let updatedState = f state
                 in parse $ update updatedState

{- if we have the space, just increment the pointer. Otherwise, add 1000 more
cells to memory -}
incrementPointer :: BFState -> BFState
incrementPointer state
    | state^.pointer < S.length (state^.memory) = state & pointer +~ 1
    | otherwise = state & pointer +~ 1 & memory .~ newmem
    where newmem = (state^.memory) >< S.replicate 1000 0

{- we could potentially expand the array to the left, but it would be illogical
for the instruction pointer to go below zero, so I don't. -}
decrementPointer :: BFState -> BFState
decrementPointer state
    | state^.pointer > 0 = state & pointer -~ 1
    | otherwise = error "Pointer lowered below zero error"

incrementValue :: BFState -> BFState
incrementValue state = state & memory .~ newMem
    where newMem = S.adjust (+1) (state^.pointer) (state^.memory)

decrementValue :: BFState -> BFState
decrementValue state = state & memory .~ newMem
    where newMem = S.adjust (+ (-1)) (state^.pointer) (state^.memory)

printValue :: BFState -> IO ()
printValue state
    | val >= 0 = putChar $ chr val
    | otherwise = error "Value attempted to print is less than 0 error"
    where val = fromIntegral $ S.index (state^.memory) (state^.pointer)

getCharacter :: BFState -> IO BFState
getCharacter state = do
    end <- isEOF
    if end
        then return state
        else do
            c <- fromIntegral . ord <$> getChar
            let newMem = S.update (state^.pointer) c (state^.memory)
            return $ state & memory .~ newMem

{- we use tail here so the first [ gets chopped off. Otherwise, the counter
will be incorrectly incremented -}
startLoop :: BFState -> BFState
startLoop state
    | S.index (state^.memory) (state^.pointer) == 0
      = state & nonConsumedInput .~ newNonConsumed
    | otherwise = state
    where newNonConsumed = nextBracket (tail $ state^.nonConsumedInput) 0

{- to handle nested brackets while looping, each time we encounter a [, we 
increment our count by 1. If we meet a ] and the counter is not 0, then we 
have more layers of nesting to leave. If we get to the end of the input
then the programmer must have made a mistake so we throw an error. -}
nextBracket :: String -> Int -> String
nextBracket [] _ = error "Mismatched [ error"
nextBracket z@(x:xs) count
    | x == ']' && count == 0 = z
    | x == ']' = nextBracket xs (count - 1)
    | x == '[' = nextBracket xs (count + 1)
    | otherwise = nextBracket xs count

endLoop :: BFState -> BFState
endLoop state
    | S.index (state^.memory) (state^.pointer) /= 0 = previousBracket state
    | otherwise = state

{- we reverse the input before the nonconsumedinput as we need to search
backwards and this makes it a lot easier. -}
previousBracket :: BFState -> BFState
previousBracket state = state & nonConsumedInput .~ find search n 0
    where search = reverse $ take (length t - length n) t
          find [] _ _ = error "Mismatched ] error"
          find (x:xs) acc count
            | x == '[' && count == 0 = x : acc
            | x == '[' = find xs (x:acc) (count - 1)
            | x == ']' = find xs (x:acc) (count + 1)
            | otherwise = find xs (x:acc) count
          t = state^.totalInput
          n = state^.nonConsumedInput

--remove one character from the nonconsumedinput - it has just been parsed.
update :: BFState -> BFState
update state
    | null $ state^.nonConsumedInput = state
    | otherwise = state & nonConsumedInput .~ tail (state^.nonConsumedInput)

interpreter :: IO ()
interpreter = putStrLn msg >> interpreter'
    where msg = ">>> Brainfuck interpreter (Ctrl+D to exit \
                \/ Ctrl+Z on windows) <<<"
    
interpreter' :: IO ()
interpreter' = do
    putStr "bf: "
    hFlush stdout
    end <- isEOF
    if end 
        then putStrLn ""
        else do
            input <- getLine
            parse' $ BFState byteArray 0 input input
            interpreter'
