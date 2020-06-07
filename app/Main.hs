module Main where

import Lib

import Debug.Trace

dat :: String
dat = "123+45*67"

-- dat :: [Int]
-- dat = [10,20,30,40,50]

nth :: Int -> [a] -> a
nth n l = head (drop n l)

parse :: [Char] -> [Char]
parse [] = [nonval]
parse acc =
  let h =  (trace ("ZZZZZZ " ++ show acc)) (head acc)
  in
    if  False
    then [h ] ++ (drop 1 acc)
    else [nonval] ++ acc

nonval :: Char
nonval = 'Z'

none :: [Char] -> Bool
none acc = (length acc >= 1) && (head acc) == nonval

eat :: Int -> [Char] -> [Char]
eat n acc =
  if (n > (length dat))
  then acc
  else (if (none acc)
        then eat (1 + n) (foo n acc)
        else eat n (parse acc))

foo n acc =
  ((if (n == (length dat))
                           then ['E']
                           else [dat !! n]) ++ (drop 1 acc))

main :: IO ()
main = do
  beginFunc
  putStrLn "in  the middle"
  putStrLn (show dat)
  endFunc
