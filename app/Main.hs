module Main where

import Lib

main :: IO ()
main = do
  beginFunc
  putStrLn "in the middle"
  endFunc
