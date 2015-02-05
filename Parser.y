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

Expr        : App sp1 '\\' sp Var sp '.' sp Expr    { App1 (App $1 (Nested (Lambda $5 $9))) }
            | '\\' sp Var sp '.' sp Expr            { Lambda $3 $7 }
            | App                                   { App1 $1 }

App         : App sp1 Atom                          { App $1 $3 }
            | Atom                                  { Atom1 $1 }

Atom        : '(' Expr ')'                          { Nested $2 }
            | Var                                   { Var1 $1 }

Var         : var                                   { Var $1 }

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

data Expr 
        = Lambda Var Expr
        | App1 App

data App
        = App App Atom
        | Atom1 Atom

data Atom
        = Nested Expr
        | Var1 Var

data Var
        = Var String

{- Instances -}

instance Read Expr where
    readsPrec _ s = [(parse $ lexer $ dropWhile isSpace s, "")]

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
