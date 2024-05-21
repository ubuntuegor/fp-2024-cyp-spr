module Interpreter where

import Control.Monad.Except
import Control.Monad.State
import Data.List (find)
import Parser
import System.IO (Handle, hPrint)

data Env = Env {funcs :: [Func], vars :: [(String, Int)], handle :: Handle}

type EvalResult = ExceptT String (StateT Env IO) Int

evalBinop :: Op -> Expr -> Expr -> EvalResult
evalBinop op lhs rhs = do
  lhs' <- evalExpr lhs
  rhs' <- evalExpr rhs
  case op of
    Add -> return $ lhs' + rhs'
    Sub -> return $ lhs' - rhs'
    Mul -> return $ lhs' * rhs'
    Div ->
      if rhs' == 0
        then throwError "Divide by zero"
        else return $ lhs' `div` rhs'
    Mod ->
      if rhs' == 0
        then throwError "Divide by zero"
        else return $ lhs' `mod` rhs'
    Eq -> return $ fromEnum $ lhs' == rhs'
    Ne -> return $ fromEnum $ lhs' /= rhs'
    Gt -> return $ fromEnum $ lhs' > rhs'
    Ge -> return $ fromEnum $ lhs' >= rhs'
    Lt -> return $ fromEnum $ lhs' < rhs'
    Le -> return $ fromEnum $ lhs' <= rhs'
    And -> return $ fromEnum $ lhs' /= 0 && rhs' /= 0
    Or -> return $ fromEnum $ lhs' /= 0 || rhs' /= 0

evalExpr :: Expr -> EvalResult
evalExpr expr =
  case expr of
    Const n -> return n
    Var x -> do
      vars' <- gets vars
      let result = lookup x vars'
      case result of
        Just n -> return n
        Nothing -> throwError $ "Undefined variable: " ++ x
    Call f args -> do
      funcs' <- gets funcs
      let result = find (\(Func name _ _) -> name == f) funcs'
      case result of
        Just func@(Func _ params _) ->
          if length params /= length args
            then throwError $ "Insufficient number of arguments passed to function " ++ f
            else callFunc func args
        Nothing -> throwError $ "Undefined function: " ++ f
    Binop op lhs rhs -> evalBinop op lhs rhs
  where
    callFunc (Func _ params body) args = do
      args' <- mapM evalExpr args
      let newVars = zip params args'
      vars' <- gets vars
      modify (\env -> env {vars = newVars})
      value <- evalStmt body
      modify (\env -> env {vars = vars'})
      return value

evalStmt :: Stmt -> EvalResult
evalStmt stmt =
  case stmt of
    Asgn name expr -> do
      value <- evalExpr expr
      modify (\env -> env {vars = (name, value) : vars env})
      return value
    Expr' expr -> evalExpr expr
    Write expr -> do
      value <- evalExpr expr
      handle' <- gets handle
      liftIO $ hPrint handle' value
      return value
    Read name -> do
      value <- liftIO getLine
      modify (\env -> env {vars = (name, read value) : vars env})
      return 0
    While cond body -> runWhile cond body
    If cond body elseBody -> do
      condValue <- evalExpr cond
      if condValue /= 0
        then evalStmt body
        else case elseBody of
          Just elseBody' -> evalStmt elseBody'
          Nothing -> return 0
    Skip -> return 0
    Seq stmt1 stmt2 -> evalStmt stmt1 *> evalStmt stmt2
  where
    runWhile cond body = do
      condValue <- evalExpr cond
      if condValue /= 0
        then evalStmt body *> runWhile cond body
        else return 0

evalProgram :: Program -> EvalResult
evalProgram (Program funcs' body) = do
  modify (\env -> env {funcs = funcs'})
  evalStmt body
