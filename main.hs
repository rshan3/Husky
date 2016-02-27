{-----------------------------------
 - Interpreter.hs
 - v1.1
 -----------------------------------}

module Interpreter where

-- Language Representation
import Data.HashMap.Strict as H
import Text.ParserCombinators.Parsec
import Parser

liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal $ op x y
liftBool _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal $ op x y
liftCompOp _ _ _ = ExnVal "Cannot lift"

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


{-----------------------------------
 - eval: The Evaluator
 -----------------------------------}
eval :: Exp -> Env -> Val

eval (IntExp k) env = IntVal k

eval (BoolExp e) env = BoolVal e

eval (VarExp e) env =
   case H.lookup e env of
     Just v -> v
     Nothing -> ExnVal "No match in env"

eval (IntOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = H.lookup op intOps
  in liftIntOp f v1 v2

eval (BoolOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = H.lookup op boolOps
  in liftBoolOp f v1 v2

eval (CompOpExp op e1 e2) env =
  let v1 = eval e1 env
      v2 = eval e2 env
      Just f = H.lookup op compOps
  in liftCompOp f v1 v2

--TODO: Handling functions and closures


{-----------------------------------
 - exec
 - env is the map of current available var
 - penv is for the procedure environment
 -----------------------------------}
exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
   where val = show $ eval e env

exec (SeqStmt [s]) penv env = exec s penv env
exec (SeqStmt (x:xs)) penv env =
    let (p1,penv1,env1) = exec x penv env
        (p2,penv2,env2) = exec (SeqStmt xs) penv1 env1
    in (p1++p2,penv2,env2)

exec (IfStmt e1 s2 s3) penv env =
    case eval e1 env of
      BoolVal True -> exec s2 penv env
      _ -> exec s3 penv env

exec (SetStmt var s) penv env =
    let v = eval s env
    in ("",penv,H.insert var v env)

--TODO: Call Statement execution

{-----------------------------------
 - repl
 -----------------------------------}
repl :: PEnv -> Env -> [String] -> String -> IO Result
repl penv env [] _ =
  do putStr "> "
     input <- getLine
     case parse stmt "stdin" input of
        Right QuitStmt -> do putStrLn "Bye Bye!"
                             return ("",penv,env)
        Right x -> let (nuresult,nupenv,nuenv) = exec x penv env
                   in do {
                     putStrLn nuresult;
                     repl nupenv nuenv [] "stdin"
                   }
        Left x -> do putStrLn $ show x
                     repl penv env [] "stdin"

main = do
  putStrLn "Welcome to Husky!"
  repl H.empty H.empty [] "stdin"
