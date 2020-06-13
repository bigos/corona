module Main where

import Debug.Trace
import Data.Char

import Lib

type TypeName = String

data Foo = EndVal | NonVal | C Char | S String | D Int | Node (Foo) deriving (Eq, Show)

-- testing function
qqq :: Foo -> String
qqq a =
  case a of
    EndVal -> "this is END val"
    NonVal -> "this is NON val"
    C c    -> "this is C Char "   ++ show c
    S s    -> "this is S String " ++ show s
    D i    -> "this is D Int "    ++ show i
    Node n -> "this is Node "     ++ show n






dat :: String
dat = "123+45*67"

nth :: Int -> [a] -> a
nth n l = head (drop n l)

nextOrEndVal :: Int -> [Foo]
nextOrEndVal n =
  if n == (length dat)
  then [EndVal]
  else [C (dat !! n)]

none :: [Foo] -> Bool
none [] = trace ("first el of " ++ show dat)  False
none acc = head acc == NonVal

eat :: Int -> [Foo] -> [Foo]
eat n acc =
  if trace ("acc is === " ++ show acc) $ (n > (length dat))
  then acc
  else eat2
  where
    eat2 = if (none acc)
           then trace ("TTT " ++ show acc) $ eat (1 + n) (nextOrEndVal n) ++ (drop 1 acc)
           else trace ("EEE " ++ show acc) $ eat n       (parse acc)

parse :: [Foo] -> [Foo]
parse [] = [NonVal]
parse acc =
  let
    h = head acc
    res = case h of
            C c -> (if (isDigit c)
                    then [Node (D (digitToInt c)) ] ++ (drop 1 acc)
                    else [Node (C c)] ++ (drop 1 acc))
            _   -> [NonVal] ++ acc
  in
    trace ("res is " ++ show res) $ res

main :: IO ()
main = do
  beginFunc
  putStrLn "in  the middle"
  putStrLn (show dat)
  endFunc
