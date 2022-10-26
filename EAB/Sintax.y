{
module EAB.Sintax where
import Data.Char
import Data.List
}

%name parser
%tokentype { Token }
%error { parseError }

%token
    let     { TokenLet }
    in      { TokenIn }
    end     { TokenEnd }
    '='     { TokenAssign }
    or    { TokenOr }
    and    { TokenAnd }
    '+'     { TokenPlus }
    '*'     { TokenTimes }
    not     { TokenNot }
    '('     { TokenLParen }
    ')'     { TokenRParen }
    var     { TokenVar $$ }
    num     { TokenNum $$ }
    bol     { TokenBol $$ }

%right in
%left or and 
%right not
%left '+'
%left '*'
%%

EAB : let var '=' EAB in EAB end    {Let $2 $4 $6}
    | EAB or EAB                  {Or $1 $3}
    | EAB and EAB                  {And $1 $3}
    | EAB '+' EAB                   {Plus $1 $3}
    | EAB '*' EAB                   {Times $1 $3}
    | not EAB                       {Not $2}
    | '(' EAB ')'                   {$2}
    | var                           {Var $1}
    | num                           {Num $1}
    | bol                           {Bol $1}


{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data EAB = Let String EAB EAB
         | Or EAB EAB
         | And EAB EAB
         | Plus EAB EAB
         | Times EAB EAB
         | Not EAB
         | Var String
         | Num Int
         | Bol Bool
         deriving (Show , Eq)

data Token = TokenLet
           | TokenIn
           | TokenEnd
           | TokenVar String
           | TokenNum Int
           | TokenAssign
           | TokenOr
           | TokenAnd
           | TokenPlus
           | TokenTimes
           | TokenNot
           | TokenLParen
           | TokenRParen
           | TokenBol Bool
           deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
        | isSpace c = lexer cs
        | isAlpha c = lexVar (c:cs)
        | isDigit c = lexInt (c:cs)
lexer ('=':cs) = TokenAssign : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs

lexInt :: String -> [Token]
lexInt cs = TokenNum (read n) : lexer rest
    where (n, rest) = span isDigit cs

lexVar :: String -> [Token]
lexVar cs = case span isAlpha cs of
                ("let", cs') -> TokenLet : lexer cs'
                ("in", cs') -> TokenIn : lexer cs'
                ("end", cs') -> TokenEnd : lexer cs'
                ("not", cs') -> TokenNot : lexer cs'
                ("true", cs') -> TokenBol True : lexer cs'
                ("false", cs') -> TokenBol False : lexer cs'
                ("or", cs') -> TokenOr : lexer cs'
                ("and", cs') -> TokenAnd : lexer cs'
                (var, cs') -> TokenVar var : lexer cs'

}