module Main where

import Utils
import Control.Monad
import Data.List
import Data.List.Split

showSystem :: Maybe System -> String
showSystem Nothing = "Unable to solve the system"
showSystem (Just eqs) = intercalate "\n" $ map (\(a, b) -> show a ++ " = " ++ show b) eqs

main = do
    linestr <- liftM lines (readFile "task6.in")
    let eqstr = map (splitOn "=") linestr
    let eqs = map (\[a, b] -> (read a, read b)) eqstr
    writeFile "task6.out" $ showSystem (solve eqs) ++ "\n"
