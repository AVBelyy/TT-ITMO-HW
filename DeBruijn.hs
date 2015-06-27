module DeBruijn where

import qualified Parser as P

data DBExpr = Lambda DBExpr
            | App DBExpr DBExpr
            | Var Int
            deriving Eq

type Context = Int -> Int -> DBExpr
type DBSubst = Int -> String
type Subst = String -> Int

findBound :: DBExpr -> Int -> DBExpr
findBound (Lambda m)  d = Lambda $ findBound m (d + 1)
findBound (App a b)   d = App (findBound a d) (findBound b d)
findBound (Var v)     d = Var $ if v == d then (-v) else v

updFree :: DBExpr -> Int -> Int -> DBExpr
updFree (Lambda m)  x d = Lambda $ updFree m x (d + 1)
updFree (App a b)   x d = App (updFree a x d) (updFree b x d)
updFree (Var v)     x d = Var $ if v > d then v + x else v

subst :: DBExpr -> DBExpr -> Int -> DBExpr
subst m@(Var v)  n d = if v < 0 then updFree n d 0 else m
subst (Lambda m) n d = Lambda $ subst m n (d + 1)
subst (App a b)  n d = App (subst a n d) (subst b n d)

eval :: DBExpr -> DBExpr
eval (App l r) = case (eval l, eval r) of
    (Var v,    n) -> Var v `App` n
    (Lambda m, n) -> eval $ subst (updFree (findBound (eval m) 1) (-1) 1) n 0
    (App a b,  n) -> a `App` b `App` n
eval (Lambda m) = Lambda $ eval m
eval m = m

-- Conversion part (P.Expr -> DBExpr)

nil :: Subst
nil x = error $ "unknown variable '" ++ x ++ "'"

append :: Subst -> String -> Int -> Subst
append ctx s v x = if x == s then v else ctx x

toDBExpr :: P.Expr -> DBExpr
toDBExpr expr = _toDBExpr expr nil 0 where
    _toDBExpr (P.Var v)      ctx d = Var $ d - ctx v
    _toDBExpr (P.Lambda v m) ctx d = Lambda $ _toDBExpr m (append ctx v d) (d + 1)
    _toDBExpr (P.App a b)    ctx d = App (_toDBExpr a ctx d) (_toDBExpr b ctx d)

-- Conversion part (DBExpr -> P.Expr)

dbnil :: DBSubst
dbnil x = error $ "unknown variable '" ++ show x ++ "'"

genVar :: Int -> String
genVar x = "xyzvt" !! q : iterate ('\'':) "" !! p where p = x `div` 5
                                                        q = x `mod` 5
dbappend :: DBSubst -> Int -> String -> DBSubst
dbappend ctx v s x = if x == v then s else ctx x

toExpr :: DBExpr -> P.Expr
toExpr expr = _toExpr expr dbnil 0 where
    _toExpr (Var v)    ctx d = P.Var $ if v <= d then ctx (d - v) else genVar (v - 1)
    _toExpr (Lambda m) ctx d = P.Lambda v $ _toExpr m (dbappend ctx d v) (d + 1) where v = genVar d
    _toExpr (App a b)  ctx d = P.App (_toExpr a ctx d) (_toExpr b ctx d)
