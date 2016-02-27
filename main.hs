

data Val = IntVal Integer
		| BoolVal Bool
		| ClosureVal String Exp Env
	deriving (Show,Eq)

data Exp = IntExp Integer
			| IntOpExp String Exp Exp
			| CompOpExp String Exp Exp
			| VarExp String
			| IfExp Exp Exp Exp
			| LetExp String Exp Exp
			| FuncExp String Exp
			| AppExp Exp Exp
	deriving (Show,Eq)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
    deriving Show

add elt Empty = Node elt Empty Empty
add elt (Node x left right)
   | elt < x   = Node x (add elt left) right
   | otherwise = Node x left (add elt right)


type Env = [(String,Val)]
type IntOpEnv = [(String,Integer -> Integer -> Integer)]

intOps = [("+",(+)),
		("-",(-)), 
		("*",(*)),
		("/",div)]

compOps = [ ("<",(<)),
			(">",(>)),
			("==",(==)),
			("<=",(<=)),
			(">=",(>=)),
			("/=",(/=))]

liftIntOp f (IntVal i1) (IntVal i2) = IntVal (f i1 i2)
liftIntOp f _ _ = IntVal 0

liftCompOp f (IntVal i1) (IntVal i2) = BoolVal (f i1 i2)
liftCompOp f _ _ = BoolVal False


eval :: Exp -> Env -> Val
eval (IntExp i) _ = IntVal i
eval (VarExp s) env =
	let Just v = lookup s env
	in v

eval (CompOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op compOps
  in liftCompOp f v1 v2

eval (IntOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = lookup op intOps
  in liftIntOp f v1 v2


eval (IfExp c t e) env =
	case (eval c env) of
		BoolVal True -> eval t env
		_ -> eval e env


eval (LetExp v e1 e2) env =
	let v1 = eval e1 env
	in eval e2 ((v,v1):env)


eval (FunExp v e1) env =
	ClosureVal v e1 env

eval (AppExp e1 e2) env =
	let ClosureVal v e3 cenv = eval e1 env
		arg = eval e2 env
	in eval e3 ((v,arg):cenv)

getInt (IntVal i) = i
getInt _ = 0


