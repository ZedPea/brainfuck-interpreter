module Utilities
( getFile
, helpFlags
, fileFlags
) where

import System.Directory (doesFileExist)
import Data.Maybe (mapMaybe)
import Data.List (elemIndex)

getFile :: [String] -> IO (Maybe FilePath)
getFile args
    | any (`elem` fileFlags) args = tryFile
    | otherwise = def
    where maybeFlag = getFlagValue args fileFlags
          tryFile = maybe def foo maybeFlag
          foo flag = do
            exists <- doesFileExist flag
            if exists then return (Just flag) else def
          def = return Nothing

helpFlags :: [String]
helpFlags = ["--help","-h"]

fileFlags :: [String]
fileFlags = ["--file","-f"]

{-
Gets the value in the argument list following one of the tags specified in 
flags if the flag exists in the argument list, and the argument list is
long enough to get the next item in the argument list
-}
getFlagValue :: [String] -> [String] -> Maybe String
getFlagValue args flags
    | flagExists && len > val = Just (args !! val)
    | otherwise = Nothing
    where len = length args
          flagExists = any (`elem` flags) args
          val = 1 + head (mapMaybe (`elemIndex` args) flags)
