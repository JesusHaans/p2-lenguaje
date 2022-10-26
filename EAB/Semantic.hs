module EAB.Semantic where
import EAB.Sintax

eval:: EAB -> Either Int Bool
eval e = case evaln e of
    (Num n) -> Left n
    (Bol b) -> Right b
    e' -> error "Exprecion bloqueada"

evaln:: EAB -> EAB
evaln e = if e == e' 
         then e 
         else evaln e'
       where e' = evalaux e

evalaux:: EAB -> EAB
evalaux (Num n) = Num n
evalaux (Bol b) = Bol b
evalaux (Var x) = error "Variable no definida"
evalaux (Let x e1 e2) = evalaux (subst e2 x (evalaux e1))
evalaux (Or e1 e2) = case (evalaux e1, evalaux e2) of
    (Bol b1, Bol b2) -> Bol (b1 || b2)
    _ -> error "Error en la operacion Or"
evalaux (And e1 e2) = case (evalaux e1, evalaux e2) of
    (Bol b1, Bol b2) -> Bol (b1 && b2)
    _ -> error "Error en la operacion And"
evalaux (Not e) = case evalaux e of
    Bol b -> Bol (not b)
    _ -> error "Error en la operacion Not"
evalaux (Plus e1 e2) = case (evalaux e1, evalaux e2) of
    (Num n1, Num n2) -> Num (n1 + n2)
    _ -> error "Error en la operacion Plus"
evalaux (Times e1 e2) = case (evalaux e1, evalaux e2) of
    (Num n1, Num n2) -> Num (n1 * n2)
    _ -> error "Error en la operacion Times"


subst:: EAB -> String -> EAB -> EAB
subst (Num n) x e = Num n
subst (Bol b) x e = Bol b
subst (Var y) x e = if x == y then e else Var y
subst (Let y e1 e2) x e = Let y (subst e1 x e) (subst e2 x e)
subst (Or e1 e2) x e = Or (subst e1 x e) (subst e2 x e)
subst (And e1 e2) x e = And (subst e1 x e) (subst e2 x e)
subst (Not e1) x e = Not (subst e1 x e)
subst (Plus e1 e2) x e = Plus (subst e1 x e) (subst e2 x e)
subst (Times e1 e2) x e = Times (subst e1 x e) (subst e2 x e)

