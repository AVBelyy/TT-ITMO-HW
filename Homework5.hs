module Main where

import Parser
import Utils

s, k, i :: Expr
s = Var "S"
k = Var "K"
i = Var "I"

transform :: Expr -> Expr
transform (Var v) = Var v
transform (App e1 e2) = App (transform e1) (transform e2)
transform (Lambda x (Var v))
    | x == v = i
transform (Lambda x (Lambda y e))
    | x `elem` freeVars e = transform $ Lambda x $ transform $ Lambda y e
transform (Lambda x (App e1 e2))
    | x `elem` freeVars e1 ++ freeVars e2 = s `App` transform (Lambda x e1) `App` transform (Lambda x e2)
transform (Lambda x e)
    | x `notElem` freeVars e = k `App` transform e


main = do
    str <- readFile "task5.in"
    let expr = read str
    writeFile "task5.out" $ show (transform (normalize expr)) ++ "\n"
