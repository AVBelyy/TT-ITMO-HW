module Main where

import Parser
import Utils ()

main = do
    str <- readFile "task1.in"
    let expr = read str
    writeFile "task1.out" $ show (Nested expr) ++ "\n"
