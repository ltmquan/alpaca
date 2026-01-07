{-|
Module      : AST
Description : Abstract syntax tree components
Copyright   : (c) lwq, 2026
License     : BSD-3 Clause
Maintainer  : ltmquan2001@gmail.com

Data types for the AST.
-}

module AST
    ( Id(..)
    , Program(..)
    , Stmt(..)
    , Type(..)
    , TypeOp(..)
    , Pat(..)
    , Expr(..)
    , BinOp(..)
    , UnaryOp(..)
    , Lit(..)
    ) where


-- ============ Identifiers ============

newtype Id = Id String
    deriving (Show, Eq)


-- ============ Program Structure ============

newtype Program = Program [Stmt]
    deriving (Show, Eq)

data Stmt
    = TypeDecl Id Type           -- id :: type
    | FuncDecl Id [Pat] Expr     -- id pat* = expr
    deriving (Show, Eq)


-- ============ Types ============

data Type
    = TLit String                -- Int, Char, Str, Bool
    | TVar Id                    -- type variable
    | TList Type                 -- [type]
    | TTuple [Type]              -- (type, type, ...)
    | TOp TypeOp Type Type       -- type op type
    deriving (Show, Eq)

data TypeOp
    = TArrow                     -- ->
    | TUnion                     -- |
    deriving (Show, Eq)


-- ============ Patterns ============

data Pat
    = PVar Id                    -- variable
    | PLit Lit                   -- literal
    | PWild                      -- _
    | PTuple [Pat]               -- (pat, pat, ...)
    | PList [Pat]                -- [pat, pat, ...]
    deriving (Show, Eq)


-- ============ Expressions ============

data Expr
    = EVar Id                    -- variable
    | ELit Lit                   -- literal
    | EApp Expr Expr             -- function application
    | EBinOp BinOp Expr Expr     -- binary operation
    | EUnaryOp UnaryOp Expr      -- unary operation
    | ETuple [Expr]              -- (expr, expr, ...)
    | EList [Expr]               -- [expr, expr, ...]
    deriving (Show, Eq)

data BinOp
    = OpOr                       -- ||
    | OpAnd                      -- &&
    | OpEq                       -- ==
    | OpNeq                      -- /=
    | OpLt                       -- <
    | OpGt                       -- >
    | OpLe                       -- <=
    | OpGe                       -- >=
    | OpAdd                      -- +
    | OpSub                      -- -
    | OpMul                      -- *
    | OpDiv                      -- /
    | OpConcat                   -- ++
    | OpCons                     -- :
    | OpIndex                    -- @
    deriving (Show, Eq)

data UnaryOp
    = OpNeg                      -- -
    | OpNot                      -- !
    deriving (Show, Eq)


-- ============ Literals ============

data Lit
    = LInt Integer
    | LChar Char
    | LStr String
    | LBool Bool
    deriving (Show, Eq)