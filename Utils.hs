module Utils where

import Parser

balanced :: String -> Bool
balanced (_:ss) = count ss 1 where
    count (_:"") 1 = True
    count (_:"") _ = False
    count _ 0 = False
    count ('(':xs) cnt = count xs (cnt + 1)
    count (')':xs) cnt = count xs (cnt - 1)
    count (_:xs) cnt = count xs cnt

brapp :: App -> String
brapp (Atom1 (Var1 (Var v))) = v
brapp app = brackets (show app)

brexpr :: Expr -> String
brexpr (App1 (Atom1 (Var1 (Var v)))) = v
brexpr expr = brackets (show expr)

brackets :: String -> String
brackets s
    | s == "" = ""
    | head s == '(', last s == ')', balanced s = s
    | otherwise = "(" ++ s ++ ")"

instance Show Expr where
    show (Lambda var expr) = "\\" ++ show var ++ "." ++ brexpr expr
    show (App1 app) = show app

instance Show App where
    show (App app atom) = brapp app ++ " " ++ show atom
    show (Atom1 atom) = show atom

instance Show Atom where
    show (Nested expr) = brexpr expr
    show (Var1 var) = show var

instance Show Var where
    show (Var var) = var
