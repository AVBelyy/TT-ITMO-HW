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
    let ans = if freeForSubst expr x theta
        then show $ subst expr x theta
        else "Нет свободы для подстановки для переменной " ++ x
    writeFile "task3.out" $ ans ++ "\n"
