{-|
Module      : Eval
Description : Evaluator for Alpaca language
Copyright   : (c) lwq, 2026
License     : BSD-3 Clause
Maintainer  : ltmquan2001@gmail.com

Evaluator for Alpaca.
-}

module Eval
    ( eval
    , evalProgram
    ) where

import Prelude hiding (lookup)
import Control.Monad (foldM)

import AST
    ( BinOp(..)
    , Expr(..)
    , Id(..)
    , Lit(..)
    , Pat(..)
    , Program(..)
    , Stmt(..)
    , UnaryOp(..)
    )
import Env
    ( Env
    , Value(..)
    , empty
    , extend
    , insert
    , lookup
    )


-- ============ Program Evaluation ============

evalProgram :: Program -> Either String Env
evalProgram (Program stmts) = foldM evalStmt empty stmts


-- ============ Statement Evaluation ============

evalStmt :: Env -> Stmt -> Either String Env
evalStmt env stmt = case stmt of
    TypeDecl _ _ -> Right env
    FuncDecl name pats body -> 
        Right (insert name (VFunc pats body env) env)


-- ============ Expression Evaluation ============

eval :: Env -> Expr -> Either String Value
eval env expr = case expr of
    ELit lit -> evalLit lit
    EVar name -> evalVar env name
    EApp e1 e2 -> evalApp env e1 e2
    EBinOp op e1 e2 -> evalBinOp env op e1 e2
    EUnaryOp op e -> evalUnaryOp env op e
    ETuple es -> evalTuple env es
    EList es -> evalList env es

evalLit :: Lit -> Either String Value
evalLit lit = case lit of
    LInt n -> Right (VInt n)
    LChar c -> Right (VChar c)
    LStr s -> Right (VStr s)
    LBool b -> Right (VBool b)

evalVar :: Env -> Id -> Either String Value
evalVar env name = case lookup name env of
    Just val -> Right val
    Nothing -> Left ("undefined variable: " ++ show name)

evalTuple :: Env -> [Expr] -> Either String Value
evalTuple env es = VTuple <$> mapM (eval env) es

evalList :: Env -> [Expr] -> Either String Value
evalList env es = VList <$> mapM (eval env) es

-- ============ Function Application Evaluation ============

evalApp :: Env -> Expr -> Expr -> Either String Value
evalApp env e1 e2 = do
    func <- eval env e1
    arg <- eval env e2
    apply func arg

apply :: Value -> Value -> Either String Value
apply func arg = case func of
    VFunc [] body closureEnv -> do
        func' <- eval closureEnv body
        apply func' arg
    VFunc (p:ps) body closureEnv -> do
        newEnv <- bindPattern (extend closureEnv) p arg
        if null ps
            then eval newEnv body
            else Right (VFunc ps body newEnv)
    _ -> Left "cannot apply non-function"

bindPattern :: Env -> Pat -> Value -> Either String Env
bindPattern env pat val = case (pat, val) of
    (PVar name, _) -> Right (insert name val env)
    (PWild, _) -> Right env
    (PLit lit, v) -> if matchLit lit v then Right env else Left "pattern match failure"
    (PTuple ps, VTuple vs) 
        | length ps == length vs -> bindPatterns env ps vs
        | otherwise -> Left "tuple size mismatch"
    (PList ps, VList vs)
        | length ps == length vs -> bindPatterns env ps vs
        | otherwise -> Left "list size mismatch"
    _ -> Left "pattern match failure"

bindPatterns :: Env -> [Pat] -> [Value] -> Either String Env
bindPatterns env [] [] = Right env
bindPatterns env (p:ps) (v:vs) = do
    env' <- bindPattern env p v
    bindPatterns env' ps vs
bindPatterns _ _ _ = Left "pattern match failure"

matchLit :: Lit -> Value -> Bool
matchLit (LInt n) (VInt m) = n == m
matchLit (LChar c) (VChar d) = c == d
matchLit (LStr s) (VStr t) = s == t
matchLit (LBool b) (VBool c) = b == c
matchLit _ _ = False

-- ============ Operation Evaluation ============

evalBinOp :: Env -> BinOp -> Expr -> Expr -> Either String Value
evalBinOp env op e1 e2 = do
    v1 <- eval env e1
    v2 <- eval env e2
    applyBinOp op v1 v2

applyBinOp :: BinOp -> Value -> Value -> Either String Value
applyBinOp op v1 v2 = case (op, v1, v2) of
    -- Arithmetic
    (OpAdd, VInt a, VInt b) -> Right (VInt (a + b))
    (OpSub, VInt a, VInt b) -> Right (VInt (a - b))
    (OpMul, VInt a, VInt b) -> Right (VInt (a * b))
    (OpDiv, VInt a, VInt b)
        | b == 0 -> Left "division by zero"
        | otherwise -> Right (VInt (a `div` b))
    
    -- Comparison
    (OpEq, VInt a, VInt b) -> Right (VBool (a == b))
    (OpNeq, VInt a, VInt b) -> Right (VBool (a /= b))
    (OpLt, VInt a, VInt b) -> Right (VBool (a < b))
    (OpGt, VInt a, VInt b) -> Right (VBool (a > b))
    (OpLe, VInt a, VInt b) -> Right (VBool (a <= b))
    (OpGe, VInt a, VInt b) -> Right (VBool (a >= b))
    
    -- Boolean
    (OpAnd, VBool a, VBool b) -> Right (VBool (a && b))
    (OpOr, VBool a, VBool b) -> Right (VBool (a || b))
    
    -- List operations
    (OpCons, x, VList xs) -> Right (VList (x : xs))
    (OpConcat, VList xs, VList ys) -> Right (VList (xs ++ ys))
    (OpIndex, VList xs, VInt i)
        | i < 0 || i >= fromIntegral (length xs) -> Left "index out of bounds"
        | otherwise -> Right (xs !! fromIntegral i)
    
    -- String concat
    (OpConcat, VStr s1, VStr s2) -> Right (VStr (s1 ++ s2))
    
    _ -> Left "type error in binary operation"

evalUnaryOp :: Env -> UnaryOp -> Expr -> Either String Value
evalUnaryOp env op e = do
    v <- eval env e
    applyUnaryOp op v

applyUnaryOp :: UnaryOp -> Value -> Either String Value
applyUnaryOp op v = case (op, v) of
    (OpNeg, VInt n) -> Right (VInt (-n))
    (OpNot, VBool b) -> Right (VBool (not b))
    _ -> Left "type error in unary operation"
