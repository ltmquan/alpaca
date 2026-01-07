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
    , DataId(..)
    , Program(..)
    , Decl(..)
    , Guard(..)
    , DataCon(..)
    , Type(..)
    , Expr(..)
    , CaseBranch(..)
    , BinOp(..)
    , UnaryOp(..)
    , Pat(..)
    , Lit(..)
    ) where


-- ============ Identifiers ============

newtype Id = Id String           -- lowercase: variables, type variables
    deriving (Show, Eq)

newtype DataId = DataId String   -- uppercase: type constructors, data constructors
    deriving (Show, Eq)


-- ============ Program Structure ============

newtype Program = Program [Decl]
    deriving (Show, Eq)

data Decl
    = DType Id Type                        -- id :: type
    | DFunc Id [Pat] Expr (Maybe [Decl])   -- id pat* = expr [where ...]
    | DFuncGuarded Id [Pat] [Guard] (Maybe [Decl])  -- id pat* guards [where ...]
    | DData DataId [Id] [DataCon]          -- data DataId a b = Con1 | Con2
    deriving (Show, Eq)

data Guard = Guard Expr Expr             -- | cond = expr
    deriving (Show, Eq)

data DataCon = DataCon DataId [Type]     -- Con type*
    deriving (Show, Eq)


-- ============ Types ============

data Type
    = TInt                         -- Int
    | TStr                         -- Str
    | TBool                        -- Bool
    | TVar Id                      -- a (type variable, lowercase)
    | TCon DataId [Type]           -- Maybe a, Either a b (uppercase)
    | TList Type                   -- [a]
    | TTuple [Type]                -- (a, b, c)
    | TFun Type Type               -- a -> b
    | TUnion Type Type             -- a | b
    deriving (Show, Eq)


-- ============ Expressions ============

data Expr
    = EVar Id                      -- x (lowercase)
    | ECon DataId                  -- Just, Nothing (uppercase)
    | ELit Lit                     -- 1, "hello", true
    | EApp Expr Expr               -- f x
    | EBinOp BinOp Expr Expr       -- a + b
    | EUnaryOp UnaryOp Expr        -- -a, !a
    | ETuple [Expr]                -- (a, b, c)
    | EList [Expr]                 -- [1, 2, 3]
    | ECase [CaseBranch]           -- { pat -> expr, ... }
    | ELambda Pat Expr             -- pat :-> expr
    | ELet Id Expr Expr            -- let x = e1 in e2
    deriving (Show, Eq)

data CaseBranch = CaseBranch Pat Expr    -- pat -> expr
    deriving (Show, Eq)

data BinOp
    = OpOr                         -- ||
    | OpAnd                        -- &&
    | OpEq                         -- ==
    | OpNeq                        -- /=
    | OpLt                         -- 
    | OpGt                         -- >
    | OpLe                         -- <=
    | OpGe                         -- >=
    | OpAdd                        -- +
    | OpSub                        -- -
    | OpMul                        -- *
    | OpDiv                        -- /
    | OpMod                        -- %
    deriving (Show, Eq)

data UnaryOp
    = OpNeg                        -- -
    | OpNot                        -- !
    deriving (Show, Eq)


-- ============ Patterns ============

data Pat
    = PVar Id                      -- x (lowercase)
    | PLit Lit                     -- 1, "hello", true
    | PWild                        -- _
    | PCon DataId [Pat]            -- Just x, Nothing (uppercase)
    | PTuple [Pat]                 -- (a, b, c)
    | PList [Pat]                  -- [x, y, z]
    deriving (Show, Eq)


-- ============ Literals ============

data Lit
    = LInt Integer
    | LStr String
    | LBool Bool
    deriving (Show, Eq)