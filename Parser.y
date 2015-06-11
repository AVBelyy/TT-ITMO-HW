{
module Parser where

import Data.Char
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    var             { TokenVar $$ }
    '\\'            { TokenSlash }
    '.'             { TokenDot }
    '('             { TokenLpar }
    ')'             { TokenRpar }
    space           { TokenSpace }

%%

Expr        : App sp1 '\\' sp Var sp '.' sp Expr    { RuleApp1 (RuleApp $1 (RuleNested (RuleLambda $5 $9))) }
            | '\\' sp Var sp '.' sp Expr            { RuleLambda $3 $7 }
            | App                                   { RuleApp1 $1 }

App         : App sp1 Atom                          { RuleApp $1 $3 }
            | Atom                                  { RuleAtom1 $1 }

Atom        : '(' Expr ')'                          { RuleNested $2 }
            | Var                                   { RuleVar1 $1 }

Var         : var                                   { RuleVar $1 }

sp          : {- empty -}                           { [] }
            | space                                 { [] }

sp1         : space                                 { [] }

{
{- Error function -}

parseError :: [Token] -> a
parseError _ = error "Parse error"

{- Lexems -}

data Token
        = TokenVar String
        | TokenSlash
        | TokenDot
        | TokenLpar
        | TokenRpar
        | TokenSpace
        deriving Show

{- Grammar -}

data RuleExpr 
        = RuleLambda RuleVar RuleExpr
        | RuleApp1 RuleApp

data RuleApp
        = RuleApp RuleApp RuleAtom
        | RuleAtom1 RuleAtom

data RuleAtom
        = RuleNested RuleExpr
        | RuleVar1 RuleVar

data RuleVar
        = RuleVar String

{- Data structure -}

data Expr
        = Lambda String Expr
        | App Expr Expr
        | Var String
    deriving (Eq)

{- Instances & helpers -}

toExpr :: RuleExpr -> Expr
toExpr (RuleLambda (RuleVar v) expr) = Lambda v (toExpr expr)
toExpr (RuleApp1 (RuleApp app atom)) = App (toExpr (RuleApp1 app)) (toExpr (RuleApp1 (RuleAtom1 atom)))
toExpr (RuleApp1 (RuleAtom1 (RuleNested expr))) = toExpr expr
toExpr (RuleApp1 (RuleAtom1 (RuleVar1 (RuleVar v)))) = Var v

instance Read Expr where
    readsPrec _ s = [(toExpr $ parse $ lexer $ dropWhile isSpace s, "")]

{- Lexer -}

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c, cs == [] = []
    | isSpace c, cs /= [] = case head cs of
        ' ' -> lexer cs
        ')' -> lexer cs
        otherwise -> TokenSpace : lexer cs
    | isAlpha c = TokenVar var : lexer rest
        where var = c : ds ++ as
              (ds, ccs) = span isDigit cs
              (as, rest) = span (== '\'') ccs
lexer ('\\':cs) = TokenSlash : lexer cs
lexer ('.':cs) = TokenDot : lexer cs
lexer ('(':cs) = TokenLpar : lexer ccs
    where (_, ccs) = span isSpace cs
lexer (')':cs) = TokenRpar : lexer cs
}
