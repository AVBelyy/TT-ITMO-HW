module Utils where

import Control.Applicative
import Data.Functor
import Data.List
import Parser

freeVars :: Expr -> [String]
freeVars (Lambda v expr) = freeVars expr \\ [v]
freeVars (App expr1 expr2) = freeVars expr1 ++ freeVars expr2
freeVars (Var v) = [v]

subst :: Expr -> String -> Expr -> Maybe Expr
subst e@(Var v) x expr
    | v == x = Just expr
    | otherwise = Just e
subst (App expr1 expr2) x expr = App <$> subst expr1 x expr <*> subst expr2 x expr
subst e@(Lambda v lexpr) x expr
    | v == x = Just e
    | v `notElem` freeVars expr = Lambda v <$> subst lexpr x expr
    | otherwise = Nothing

subst' :: Expr -> String -> Expr -> Expr
subst' e@(Var v) x expr
    | v == x = expr
    | otherwise = e
subst' (App expr1 expr2) x expr = subst' expr1 x expr `App` subst' expr2 x expr
subst' e@(Lambda v lexpr) x expr
    | v == x = e
    | v `elem` freeVars expr, x `elem` freeVars lexpr = Lambda z (subst' (subst' lexpr v (Var z)) x expr)
    | otherwise = Lambda v (subst' lexpr x expr)
    where z = unused v (freeVars lexpr ++ freeVars expr)

unused :: String -> [String] -> String
unused var vars = if not (var `elem` vars) then var else unused (var ++ "'") vars

balanced :: String -> Bool
balanced (_:ss) = count ss 1 where
    count (_:"") 1 = True
    count (_:"") _ = False
    count _ 0 = False
    count ('(':xs) cnt = count xs (cnt + 1)
    count (')':xs) cnt = count xs (cnt - 1)
    count (_:xs) cnt = count xs cnt

eval :: Expr -> Expr
eval (App expr1 expr2) = case eval expr1 of
    Lambda v lexpr -> eval (subst' lexpr v expr2)
    expr -> expr `App` eval expr2
eval (Lambda v lexpr) = Lambda v (eval lexpr)
eval (Var v) = Var v

brackets :: Expr -> String
brackets (Var v) = v
brackets expr
    | head s == '(', last s == ')', balanced s = s
    | otherwise = "(" ++ s ++ ")"
      where s = show expr

instance Show Expr where
    show (Lambda var expr) = "\\" ++ var ++ "." ++ brackets expr
    show (App expr1 expr2) = brackets expr1 ++ " " ++ brackets expr2
    show (Var var) = var
