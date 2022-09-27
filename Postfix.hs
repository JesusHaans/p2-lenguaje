module Postfix where


-- Stack of postfix expressions
data Stack a = Top a (Stack a) | Mt


-- Data of postfix expressions
data Pgm = Postfix Int [Com]

data Com =  Num Int | Add | Div | Mul | Sub | Rem | Eq | Gt | Lt | Nget | Pop | Sel | Swap 

-- Data of tokens for the parser
data Token = TokPost | TokNum Int | TokAdd | TokDiv | TokMul | TokSub | TokRem | TokEq | TokGt | TokLt | TokNget | TokPop | TokSel | TokSwap 




-- lexer for transforming a string into a array of tokens
lexer :: String -> [Token]
lexer [] = []
lexer ('p':'o':'s':'t':'f':'i':'x':xs) = TokPost : lexer xs
lexer (' ':xs) = lexer xs
lexer ('a':'d':'d':xs) = TokAdd : lexer xs
lexer ('d':'i':'v':xs) = TokDiv : lexer xs
lexer ('m':'u':'l':xs) = TokMul : lexer xs
lexer ('s':'u':'b':xs) = TokSub : lexer xs
lexer ('r':'e':'m':xs) = TokRem : lexer xs
lexer ('e':'q':xs) = TokEq : lexer xs
lexer ('g':'t':xs) = TokGt : lexer xs
lexer ('l':'t':xs) = TokLt : lexer xs
lexer ('n':'g':'e':'t':xs) = TokNget : lexer xs
lexer ('p':'o':'p':xs) = TokPop : lexer xs
lexer ('s':'e':'l':xs) = TokSel : lexer xs
lexer ('s':'w':'a':'p':xs) = TokSwap : lexer xs
lexer (x:xs) | x >= '0' && x <= '9' = TokNum (read (takeNum (x:xs)) :: Int) : lexer (drop (length (takeNum (x:xs))) xs)

-- auxiliary function for knowig the next number on string
takeNum:: String -> String
takeNum (' ':xs) = []
takeNum (x:xs) = if x >= '0' && x <= '9' then x : takeNum xs else error "Not a number"

-- auxiliary funtion for know if a char is a digit
isDigit :: Char -> Bool
isDigit x = x >= '0' && x <= '9'

-- parser for transforming a array of tokens into a postfix expression
parser :: [Token] -> Pgm
parser [] =  error "Empty program"
parser (TokPost : (TokNum x) : xs) = Postfix x (parserCom xs)




parserCom :: [Token] -> [Com]
parserCom [] = []
parserCom (x:xs) = case x of
    TokNum n -> Num n : parserCom xs
    TokAdd -> Add : parserCom xs
    TokDiv -> Div : parserCom xs
    TokMul -> Mul : parserCom xs
    TokSub -> Sub : parserCom xs
    TokRem -> Rem : parserCom xs
    TokEq -> Eq : parserCom xs
    TokGt -> Gt : parserCom xs
    TokLt -> Lt : parserCom xs
    TokNget -> Nget : parserCom xs
    TokPop -> Pop : parserCom xs
    TokSel -> Sel : parserCom xs
    TokSwap -> Swap : parserCom xs
    _ -> error "Not a valid token"


-- interpreter for executing a postfix expression
eval :: Stack Int -> Pgm -> Int
eval s (Postfix i x) = if i == (size s) then evalAux s (Postfix i x) else error "Stack size is not correct"

evalAux :: Stack Int -> Pgm -> Int
evalAux (Top n (s)) (Postfix i []) = n
evalAux s (Postfix i ((Num n) : xs)) = evalAux (Top n (s)) (Postfix i xs)
evalAux (Top n (Top m (s))) (Postfix i (Add : xs)) = evalAux (Top (m + n) (s)) (Postfix i xs)
evalAux (Top n (Top m (s))) (Postfix i (Sub : xs)) = evalAux (Top (m - n) (s)) (Postfix i xs)
evalAux (Top n (Top m (s))) (Postfix i (Mul : xs)) = evalAux (Top (m * n) (s)) (Postfix i xs)
evalAux (Top n (Top m (s))) (Postfix i (Div : xs)) = evalAux (Top (m `div` n) (s)) (Postfix i xs)
evalAux (Top n (Top m (s))) (Postfix i (Rem : xs)) = evalAux (Top (m `mod` n) (s)) (Postfix i xs)
evalAux (Top n (Top m (s))) (Postfix i (Eq : xs)) = evalAux (Top (if m == n then 1 else 0) (s)) (Postfix i xs)
evalAux (Top n (Top m (s))) (Postfix i (Gt : xs)) = evalAux (Top (if m > n then 1 else 0) (s)) (Postfix i xs)
evalAux (Top n (Top m (s))) (Postfix i (Lt : xs)) = evalAux (Top (if m < n then 1 else 0) (s)) (Postfix i xs)
evalAux (Top n (s)) (Postfix i (Pop : xs)) = evalAux s (Postfix i xs)
evalAux (Top n (Top m (s))) (Postfix i (Swap : xs)) = evalAux (Top m (Top n (s))) (Postfix i xs)
evalAux (Top n (Top m (Top l (s)))) (Postfix i (Sel : xs)) = evalAux (Top (if l == 0 then n else m) (s)) (Postfix i xs)
evalAux (Top n (s)) (Postfix i (Nget : xs)) = evalAux (if 1 < n && n < (size s) then (Top (get n s) (s)) else s) (Postfix i xs)

-- auxiliary function to get i-th element of a stack
get :: Int -> Stack Int -> Int
get 1 (Top n (s)) = n
get i (Top n (s)) = get (i - 1) s

-- auxiliary function to get the size of a stack
size :: Stack Int -> Int
size (Top n (s)) = 1 + size s
size Mt = 0


-- Postfix.eval (Top 4 (Top 2 (Top 3 (Top 2 Mt)))) (Postfix.parser $ Postfix.lexer "postfix 4 add mul sub")