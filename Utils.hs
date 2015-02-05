module Utils where

import Parser

import qualified Data.Set as S
import qualified Data.List as L

uniquify = S.toList . S.fromList

freeVars :: Expr -> [String]
freeVars = L.sort . uniquify . doSearch []
    where doSearch bound (Lambda v expr) = doSearch (v : bound) expr
          doSearch bound (App expr1 expr2) = doSearch bound expr1 ++ doSearch bound expr2
          doSearch bound (Var v) = if v `elem` bound then [] else [v]

allVars :: Expr -> [String]
allVars = uniquify . doSearch
    where doSearch (Lambda _ expr) = doSearch expr
          doSearch (App expr1 expr2) = doSearch expr1 ++ doSearch expr2
          doSearch (Var v) = [v]

subst :: Expr -> String -> Expr -> Expr
subst start haystack needle = doSubst [] start
    where doSubst bound (Lambda v expr) = Lambda v (doSubst (v : bound) expr)
          doSubst bound (App expr1 expr2) = App (doSubst bound expr1) (doSubst bound expr2)
          doSubst bound (Var v) = if not (v `elem` bound) && v == haystack then needle else (Var v)

freeForSubst :: Expr -> String -> Expr -> Bool
freeForSubst expr x theta = (x `elem` freeVars expr) <= thetaVarsSet `S.isSubsetOf` freeVarsSet
    where thetaVarsSet = S.fromList $ allVars theta
          freeVarsSet  = S.fromList $ freeVars $ subst expr x theta

balanced :: String -> Bool
balanced (_:ss) = count ss 1 where
    count (_:"") 1 = True
    count (_:"") _ = False
    count _ 0 = False
    count ('(':xs) cnt = count xs (cnt + 1)
    count (')':xs) cnt = count xs (cnt - 1)
    count (_:xs) cnt = count xs cnt

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
