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

subst' :: Expr -> String -> String -> Expr
subst' (Var x) from to = Var $ if x == from then to else x
subst' (Lambda x p) from to = Lambda (if x == from then to else x) (subst' p from to)
subst' (App p q) from to = App (subst' p from to) (subst' q from to)

freeForSubst :: Expr -> String -> Expr -> Bool
freeForSubst expr x theta = (x `elem` freeVars expr) <= thetaVarsSet `S.isSubsetOf` freeVarsSet
    where thetaVarsSet = S.fromList $ allVars theta
          freeVarsSet  = S.fromList $ freeVars $ subst expr x theta

unused :: String -> [String] -> String
unused var vars = if not (var `elem` vars) then var else unused (var ++ "'") vars

alphaEquiv :: Expr -> Expr -> Bool
alphaEquiv (Var x) (Var y) = x == y
alphaEquiv (Lambda x p) (Lambda y q) = alphaEquiv (subst p x t) (subst q y t)
    where t = Var $ unused "t" $ allVars p ++ allVars q
alphaEquiv (App p q) (App r t) = alphaEquiv p r && alphaEquiv q t
alphaEquiv _ _ = False

eval :: Bool -> Expr -> Expr
eval f (App l r)  = case (eval f l, eval f r) of
    (Var v,      n) -> Var v `App` n
    (Lambda v m, n) -> eval f $ subst m v n'
        where n' = iterSubst (allVars n) n
              mvars = allVars m
              iterSubst [] expr = expr
              iterSubst (x:xs) expr = subst' (iterSubst xs expr) x (unused x mvars)
    (App a b,    n) -> a `App` b `App` n
eval f (Lambda v m) = Lambda v $ if f then eval f m else m
eval _ m            = m

evalNF = eval True
evalWHNF = eval False

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
