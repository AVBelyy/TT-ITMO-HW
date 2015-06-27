module Main where

import Parser
import Utils ()

main = do
    str <- readFile "task1.in"
    let expr = read str :: Expr
    writeFile "task1.out" $ "(" ++ show expr ++ ")\n"
