module Main where

import Parser6

import Control.Monad
import Data.List
import Data.List.Split

type System = [(Term, Term)]

vars :: Term -> [String]
vars (Var x) = [x]
vars (Function _ args) = concatMap vars args

subst :: Term -> String -> Term -> Term
subst what y to = _subst what
    where _subst (Var x) = if x == y then to else Var x
          _subst (Function f args) = Function f (map _subst args)

solve :: Int -> System -> System
solve n eqs
    | length eqs < n = eqs
solve _ [] = []
solve _ ((a, b):eqs)
    | a == b = solve 0 eqs
solve _ ((Function f as, Function g bs):eqs)
    | f /= g || length as /= length bs = error "⊥"
    | otherwise = solve 0 $ eqs ++ zip as bs
solve _ ((Function f as, Var x):eqs)
    = solve 0 ((Var x, Function f as):eqs)
solve _ ((Var x, t):eqs)
    | not (x `elem` vars t) && (or ( map (\(a, b) -> x `elem` vars a ++ vars b) eqs)) = solve 0 $ (Var x, t) : gsubst
    where gsubst = map (\(a, b) -> (subst a x t, subst b x t)) eqs
solve _ ((Var x, t@(Function _ _)):_)
    | x `elem` vars t = error "⊥"
solve _ eqs@[(Var x, t)]
    | not (x `elem` vars t) = eqs
    | otherwise = error "⊥"
solve n (a:b:eqs) = solve (n + 1) $ (b:eqs) ++ [a]

showSystem :: System -> String
showSystem eqs = intercalate "\n" $ map (\(a, b) -> show a ++ " = " ++ show b) eqs

main = do
    linestr <- liftM lines (readFile "task6.in")
    let eqstr = map (splitOn "=") linestr
    let eqs = map (\[a, b] -> (read a, read b)) eqstr
    let solution = solve 0 eqs
    writeFile "task6.out" $ showSystem solution ++ "\n"
