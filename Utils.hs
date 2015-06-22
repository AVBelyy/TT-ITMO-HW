module Utils where

import Control.Applicative
import Control.Monad.ST
import Control.Monad
import Data.Functor
import Data.STRef
import Data.List
import Parser
import qualified Data.Map as M
import qualified DeBruijn as DB
import qualified Parser6 as P6

type System = [(P6.Term, P6.Term)]
data TypedExpr = NoType | Typed Type
data Type = Simple String | Complex Type Type

freeVars :: Expr -> [String]
freeVars (Lambda v expr) = freeVars expr \\ [v]
freeVars (App expr1 expr2) = freeVars expr1 ++ freeVars expr2
freeVars (Var v) = [v]

{- lambda terms substitution -}
subst :: Expr -> String -> Expr -> Maybe Expr
subst e@(Var v) x expr
    | v == x = Just expr
    | otherwise = Just e
subst (App expr1 expr2) x expr = App <$> subst expr1 x expr <*> subst expr2 x expr
subst e@(Lambda v lexpr) x expr
    | v == x = Just e
    | v `notElem` freeVars expr = Lambda v <$> subst lexpr x expr
    | otherwise = Nothing

{- find first unused variable -}
unused :: String -> [String] -> String
unused var vars = if var `notElem` vars then var else unused (var ++ "'") vars

{- lambda pre-terms substitution with var renaming -}
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

{- multistep beta reduction in type-free calculus,
 - uses De-Bruijn conversion,
 - terminates for weakly-normalized terms -}
normalize :: Expr -> Expr
normalize t = unliberateTerm (length $ freeVars t) $ DB.toExpr $ DB.eval $ DB.toDBExpr $ liberateTerm (freeVars t) t
    where liberateTerm [] t = t
          liberateTerm (v:vs) t = Lambda v (liberateTerm vs t)
          unliberateTerm 0 t = t
          unliberateTerm n (Lambda _ t) = unliberateTerm (n - 1) t

{- unification problem solver -}
solve :: System -> Maybe System
solve = _solve 0
    where _solve n eqs
              | length eqs < n = Just eqs
          _solve _ [] = Just []
          _solve _ ((a, b):eqs)
              | a == b = _solve 0 eqs
          _solve _ ((P6.Function f as, P6.Function g bs):eqs)
              | f /= g || length as /= length bs = Nothing
              | otherwise = _solve 0 $ eqs ++ zip as bs
          _solve _ ((P6.Function f as, P6.Var x):eqs)
              = _solve 0 ((P6.Var x, P6.Function f as):eqs)
          _solve _ ((P6.Var x, t):eqs)
              | not (x `elem` _vars t) && (or ( map (\(a, b) -> x `elem` _vars a ++ _vars b) eqs)) = _solve 0 $ (P6.Var x, t) : gsubst
              where gsubst = map (\(a, b) -> (_subst x t a, _subst x t b)) eqs
          _solve _ ((P6.Var x, t@(P6.Function _ _)):_)
              | x `elem` _vars t = Nothing
          _solve _ eqs@[(P6.Var x, t)]
              | not (x `elem` _vars t) = Just eqs
              | otherwise = Nothing
          _solve n (a:b:eqs) = _solve (n + 1) $ (b:eqs) ++ [a]
          _subst y to (P6.Var x) = if x == y then to else P6.Var x
          _subst y to (P6.Function f args) = P6.Function f (map (_subst y to) args)
          _vars (P6.Var x) = [x]
          _vars (P6.Function _ args) = concatMap _vars args

{- type inference problem solver for STLC -}
infer :: Expr -> TypedExpr
infer = outputTypes . solve . constructSystem
    where outputTypes Nothing = NoType
          outputTypes (Just s) = Typed (transform (solution s))
          solution ((P6.Var "0", x):_) = x
          solution (_:ss) = solution ss
          solution [] = undefined
          transform (P6.Var v) = Simple ("t" ++ v)
          transform (P6.Function _ [t1, t2]) = Complex (transform t1) (transform t2)

{- construct system of type equations for STLC type inferring -}
constructSystem :: Expr -> System
constructSystem expr = runST $ do
    tt <- newSTRef M.empty
    vars <- newSTRef 0
    forM (flatize (substitute expr)) $ \expr -> do
        t1 <- assignVarToTerm tt vars expr
        case expr of
            Var v -> return (eq1 t1)
            Lambda v lexpr -> do
                t2 <- assignVarToTerm tt vars (Var v)
                t3 <- assignVarToTerm tt vars lexpr
                return (eq2 t1 t2 t3)
            App expr1 expr2 -> do
                t2 <- assignVarToTerm tt vars expr1
                t3 <- assignVarToTerm tt vars expr2
                return (eq2 t2 t3 t1)
      where substitute = snd . substitute' []
            substitute' used (Var x) = (used, Var x)
            substitute' used (Lambda x expr) = (used', Lambda y expr')
                where (used', expr') = substitute' (y : used) (subst' expr x (Var y))
                      y = unused x used
            substitute' used (App expr1 expr2) = (used', App expr1' expr2')
                where (used1', expr1') = substitute' used expr1
                      (used',  expr2') = substitute' used1' expr2
            flatize e@(Var _) = [e]
            flatize (Lambda x expr) = Lambda x expr : flatize expr
            flatize e@(App expr1 expr2) = e : flatize expr1 ++ flatize expr2
            eq1 a = (P6.Var (show a), P6.Var (show a))
            eq2 a b c = (P6.Var (show a), P6.Function "->" [P6.Var (show b), P6.Var (show c)])
            newTypeVar vars = readSTRef vars >>= \v -> (writeSTRef vars (v + 1)) >> return v
            assignVarToTerm tt vars expr = do
                let s = show expr
                m <- readSTRef tt
                case M.lookup s m of
                    Nothing    -> newTypeVar vars >>= \v -> (writeSTRef tt (M.insert s [v] m)) >> return v
                    Just (v:_) -> return v

{- terms and types output -}
instance Show Expr where
    show (Lambda var expr) = "\\" ++ var ++ "." ++ brackets expr
    show (App expr1 expr2) = brackets expr1 ++ " " ++ brackets expr2
    show (Var var) = var

instance Show Type where
    show (Complex t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
    show (Simple t) = t

brackets :: Expr -> String
brackets (Var v) = v
brackets expr
    | head s == '(', last s == ')', balanced s = s
    | otherwise = "(" ++ s ++ ")"
      where s = show expr

balanced :: String -> Bool
balanced (_:ss) = count ss 1 where
    count (_:"") 1 = True
    count (_:"") _ = False
    count _ 0 = False
    count ('(':xs) cnt = count xs (cnt + 1)
    count (')':xs) cnt = count xs (cnt - 1)
    count (_:xs) cnt = count xs cnt
