{
module EAB.Sintax where
}

%name parser
%tokentype { Token }
%error { parseError }

%token
    let     { TokenLet }
    in      { TokenIn }
    end     { TokenEnd }
    var     { TokenVar $$ }
    int     { TokenInt $$ }
    '='     { TokenAssign }
    '||'    { TokenOr }
    '&&'    { TokenAnd }
    '+'     { TokenPlus }
    '*'     { TokenTimes }
    not     { TokenNot }
    '('     { TokenLParen }
    ')'     { TokenRParen }
    true    { TokenTrue }
    false   { TokenFalse }

%right in
%nonassoc '||' '&&' not
%left '+'
%left '*'
%%

EAB : let var '=' EAB in EAB end    {let $2 $4 $6}
    | EAB '||' EAB                  {or $1 $3}
    | EAB '&&' EAB                  {and $1 $3}
    | EAB '+' EAB                   {plus $1 $3}
    | EAB '*' EAB                   {times $1 $3}
    | not EAB                       {not $2}
    | '(' EAB ')'                   {$2}
    | var                           {var $1}
    | int                           {int $1}
    | true                          {true}
    | false                         {false}


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
         | Int Int
         | True
         | False
         deriving (Show)

data Token = TokenLet
           | TokenIn
           | TokenEnd
           | TokenVar String
           | TokenInt Int
           | TokenAssign
           | TokenOr
           | TokenAnd
           | TokenPlus
           | TokenTimes
           | TokenNot
           | TokenLParen
           | TokenRParen
           | TokenTrue
           | TokenFalse
           deriving (Show)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
        | isSpace c = lexer cs
        | isAlpha c = lexVar (c:cs)
        | isDigit c = lexInt (c:cs)
lexer ('=':cs) = TokenAssign : lexer cs
lexer ('|':'|':cs) = TokenOr : lexer cs
lexer ('&':'&':cs) = TokenAnd : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('n':'o':'t':cs) = TokenNot : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
lexer ('t':'r':'u':'e':cs) = TokenTrue : lexer cs
lexer ('f':'a':'l':'s':'e':cs) = TokenFalse : lexer cs

lexInt :: String -> [Token]
lexInt cs = TokenInt (read n) : lexer rest
    where (n, rest) = span isDigit cs

lexVar :: String -> [Token]
lexVar cs = case span isAlpha cs of
                ("let", cs') -> TokenLet : lexer cs'
                ("in", cs') -> TokenIn : lexer cs'
                ("end", cs') -> TokenEnd : lexer cs'
                (var, cs') -> TokenVar var : lexer cs'

}