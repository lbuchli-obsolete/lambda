module Main where

import Lib
import System.IO

file :: String
file = "test/test.lmd"

main :: IO ()
main = do
  handle <- openFile file ReadMode
  contents <- hGetContents handle
  -- either id print $ parse "testfile" contents
  printResult True $ parse file contents >>= compile >>= interpret
  hClose handle
