{
module Parser6 where

import Data.Char
import Data.List
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    var             { TokenVar $$ }
    ','             { TokenComma }
    '('             { TokenLpar }
    ')'             { TokenRpar }

%%

Term    : var '(' args ')'              { Function $1 $3 }
        | var                           { Var $1 }

args    : {- empty -}                   { [] }
        | Term                          { [$1] }
        | Term ',' args                 { $1:$3 }

{
{- Error function -}

parseError :: [Token] -> a
parseError _ = error "Parse error"

{- Lexems -}

data Token
        = TokenVar String
        | TokenComma
        | TokenLpar
        | TokenRpar
        deriving Show

{- Data structure -}

data Term
        = Function String [Term]
        | Var String
        deriving Eq

{- Instances & helpers -}

instance Show Term where
    show (Var x) = x
    show (Function f args) = f ++ "(" ++ intercalate ", " (map show args) ++ ")"

instance Read Term where
    readsPrec _ s = [(parse $ lexer s, "")]

{- Lexer -}

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = TokenVar var : lexer rest
        where var = c : ds ++ as
              (ds, ccs) = span isDigit cs
              (as, rest) = span (== '\'') ccs
lexer (',':cs) = TokenComma : lexer cs
lexer ('(':cs) = TokenLpar : lexer cs
lexer (')':cs) = TokenRpar : lexer cs
}
