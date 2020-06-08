module Main where

import Lib

import Debug.Trace

type TypeName = String

data Foo = EndVal | NonVal | C Char | D String | E Int | F (String, Foo) deriving (Eq, Show)

dat :: String
dat = "123+45*67"

nth :: Int -> [a] -> a
nth n l = head (drop n l)

parse :: [Foo] -> [Foo]
parse [] = [NonVal]
parse acc =
  let h = (head acc)
  in
    if  False
    then [h ] ++ (drop 1 acc)
    else [NonVal] ++ acc

none :: [Foo] -> Bool
none [] = False
none acc = head acc == NonVal

eat :: Int -> [Foo] -> [Foo]
eat n acc =
  if (trace (show acc) (n > (length dat)))
  then acc
  else (if (none acc)
        then eat (1 + n) (foo n acc)
        else eat n       (parse acc))

foo :: Int -> [Foo] -> [Foo]
foo n acc =
  (if (n == (length dat))
   then [EndVal]
   else [C (dat !! n)]) ++ (drop 1 acc)

main :: IO ()
main = do
  beginFunc
  putStrLn "in  the middle"
  putStrLn (show dat)
  endFunc
