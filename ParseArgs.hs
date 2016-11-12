{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module ParseArgs where

import System.Console.CmdArgs

bf :: BF
bf = BF {
    file = def &= typ "FilePath" &= help "File to load and run code from"
} &= help "Run brainfuck code in an interpreter or from a file."
  &= summary "Brainfuck Interpreter v1.0"
 
data BF = BF {
    file :: FilePath
} deriving (Data,Typeable,Show)
