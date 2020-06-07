module Main where

import Lib

import Debug.Trace

-- dat :: String
-- dat = "123+45*67"

-- dat :: [Int]
-- dat = [10,20,30,40,50]

nth :: Int -> [a] -> a
nth n l = head (drop n l)

parse :: [Int] -> [Int]
parse [] = [nonval]
parse acc =
  let h =  (trace ("ZZZZZZ " ++ show acc)) (head acc)
  in
    if  even h
    then [h + 1] ++ (drop 1 acc)
    else [nonval] ++ acc

nonval :: Int
nonval = (-1)

none :: [Int] -> Bool
none acc = (length acc >= 1) && (head acc) == nonval

eat :: Int -> [Int] -> [Int]
eat n acc =
  if (n > (length dat))
  then acc
  else (if (none acc)
        then eat (1 + n) ((if (n == (length dat))
                           then [-999]
                           else [nth n dat]) ++ (drop 1 acc))
        else eat n (parse acc))

main :: IO ()
main = do
  beginFunc
  putStrLn "in  the middle"
  putStrLn (show dat)
  endFunc
