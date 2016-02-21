data Val = IntVal Integer
		| BoolVal Bool
	deriving (Show,Eq)

data Exp = IntExp Integer
			| IntOpExp String Exp Exp
			| CompOpExp String Exp Exp
			| VarExp String
	deriving (Show,Eq)

type Env = [(String,Val)]
type IntOpEnv = [(String,Integer -> Integer -> Integer)]

eval :: Exp -> Env -> Val
eval (IntExp i) _ = IntVal i
eval (VarExp s) env =
	let Just v = lookup s env
	in v

eval (IntOpExp op e1 e2) env =
	let v1 = eval e1 env
		v2 = eval e2 env
		Just f = lookup op intOps
	in liftIntOp f v1 v2

getInt (IntVal i) = i
getInt _ = 0

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