module Main where

import Parser ()
import Utils

main = do
    str <- readFile "task4.in"
    let expr = read str
    writeFile "task4.out" $ show (eval expr) ++ "\n"
