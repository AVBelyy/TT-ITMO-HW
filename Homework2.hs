module Main where

import Parser ()
import Utils

import Data.List

main = do
    str <- readFile "task2.in"
    let expr = read str
    let vars = freeVars expr
    writeFile "task2.out" $ intercalate "\n" vars ++ "\n"
