module Main where

import Parser ()
import Utils

main = do
    str <- readFile "task7.in"
    let expr = read str
    let ans = case infer expr of
                NoType     -> "Unable to infer types"
                Typed t    -> show t
    writeFile "task7.out" $ ans ++ "\n"
