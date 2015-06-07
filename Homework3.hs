module Main where

import Parser ()
import Utils

import Data.List.Split

main = do
    str <- readFile "task3.in"
    let (estr:rest:_) = splitOn "[" str
    let (x:thstr:_) = splitOn ":=" (init rest)
    let expr = read estr
    let theta = read (init thstr)
    let ans = case subst expr x theta of Just res -> show res
                                         Nothing  -> "Нет свободы для подстановки для переменной " ++ x
    writeFile "task3.out" $ ans ++ "\n"
