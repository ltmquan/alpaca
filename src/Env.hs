{-|
Module      : Env
Description : Environment for variable bindings
Copyright   : (c) lwq, 2026
License     : BSD-3 Clause
Maintainer  : ltmquan2001@gmail.com

Environment for storing variable bindings during evaluation.
-}

module Env
    ( Env
    , Value(..)
    , empty
    , lookup
    , insert
    , extend
    ) where

import Prelude hiding (lookup)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import AST (Expr, Id, Pat)


-- ============ Values ============

data Value
    = VInt Integer
    | VChar Char
    | VStr String
    | VBool Bool
    | VList [Value]
    | VTuple [Value]
    | VFunc [Pat] Expr Env    -- closure: params, body, captured environment
    deriving (Show, Eq)


-- ============ Environment ============

type Env = [Map Id Value]    -- stack of scopes, innermost first


-- | Empty environment with one empty scope
empty :: Env
empty = [Map.empty]


-- | Look up a variable, searching from innermost scope outward
lookup :: Id -> Env -> Maybe Value
lookup _ [] = Nothing
lookup name (scope:rest) =
    case Map.lookup name scope of
        Just val -> Just val
        Nothing  -> lookup name rest


-- | Insert into the innermost scope
insert :: Id -> Value -> Env -> Env
insert _ _ [] = error "empty environment"
insert name val (scope:rest) = Map.insert name val scope : rest


-- | Push a new empty scope
extend :: Env -> Env
extend env = Map.empty : env