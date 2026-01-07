{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Parser
Description : Parser for Alpaca language
Copyright   : (c) lwq, 2026
License     : BSD-3 Clause
Maintainer  : ltmquan2001@gmail.com

Megaparsec-based parser for Alpaca.
-}

module Parser
    ( parseProgram
    ) where

import Data.Text (Text)
import Data.Void (Void)
import Control.Monad.Combinators.Expr 
    ( Operator(..)
    , makeExprParser
    )
import Text.Megaparsec
    ( Parsec
    , (<|>)
    , between
    , choice
    , eof
    , errorBundlePretty
    , many
    , option
    , parse
    , some
    , sepBy
    , noneOf
    , notFollowedBy
    , try
    )
import Text.Megaparsec.Char
    ( alphaNumChar
    , char
    , lowerChar
    , space1
    )
import qualified Text.Megaparsec.Char.Lexer as L

import AST
    ( BinOp(..)
    , Expr(..)
    , Id(..)
    , Lit(..)
    , Pat(..)
    , Program(..)
    , Stmt(..)
    , Type(..)
    , TypeOp(..)
    , UnaryOp(..)
    )


-- ============ Parser Type ============

type Parser = Parsec Void Text


-- ============ Lexer Utilities ============

-- | Space consumer that consumes whitespace and comments
sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "{-" "-}")

-- | Lexeme wrapper (consumes trailing whitespace)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol parser (parses exact string and consumes trailing whitespace)
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse something between parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse something between brackets
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- ============ Program Parser ============

pProgram :: Parser Program
pProgram = sc *> (Program <$> many pStmt) <* eof

-- ============ Statement Parser ============
pStmt :: Parser Stmt
pStmt = pId >>= \name -> pTypeDecl name <|> pFuncDecl name

-- ============ Declarations Parser ============
pTypeDecl :: Id -> Parser Stmt
pTypeDecl name = symbol "::" *> (TypeDecl name <$> pType)

pFuncDecl :: Id -> Parser Stmt
pFuncDecl name = FuncDecl name <$> many pPat <* symbol "=" <*> pExpr

-- ============ Type Parser ============
pType :: Parser Type
pType = makeExprParser pAtomType typeOpTable

typeOpTable :: [[Operator Parser Type]]
typeOpTable =
    [ [ InfixR (TOp TArrow <$ symbol "->") ]
    , [ InfixR (TOp TUnion <$ symbol "|") ]
    ]

pAtomType :: Parser Type
pAtomType = choice
    [ pTypeLit
    , TList <$> brackets pType
    , pTupleType
    , TVar <$> pId
    ]

pTypeLit :: Parser Type
pTypeLit = TLit <$> choice
    [ "Int"  <$ symbol "Int"
    , "Char" <$ symbol "Char"
    , "Str"  <$ symbol "Str"
    , "Bool" <$ symbol "Bool"
    ]

pTupleType :: Parser Type
pTupleType = parens (pType >>= \first -> 
    option first (mkTuple first))
  where
    mkTuple first = 
        TTuple . (first :) <$> some (symbol "," *> pType)

-- ============ Pattern Parser ============
pPat :: Parser Pat
pPat = choice
    [ PWild <$ symbol "_"
    , PLit <$> pLit
    , pTuplePat
    , pListPat
    , PVar <$> pId
    ]

pTuplePat :: Parser Pat
pTuplePat = parens (pPat >>= \first -> 
    option first (mkTuple first))
  where
    mkTuple first = 
        PTuple . (first :) <$> some (symbol "," *> pPat)

pListPat :: Parser Pat
pListPat = PList <$> brackets (pPat `sepBy` symbol ",")

-- ============ Expression Parser ============
pExpr :: Parser Expr
pExpr = makeExprParser pAppExpr exprOpTable

exprOpTable :: [[Operator Parser Expr]]
exprOpTable =
    [ [ Prefix (EUnaryOp OpNeg <$ symbol "-")
      , Prefix (EUnaryOp OpNot <$ symbol "!")
      ]
    , [ InfixL (EBinOp OpIndex <$ symbol "@") ]
    , [ InfixL (EBinOp OpMul <$ symbol "*")
      , InfixL (EBinOp OpDiv <$ try (symbol "/" <* notFollowedBy (char '=')))
      ]
    , [ InfixL (EBinOp OpAdd <$ try (symbol "+" <* notFollowedBy (char '+')))
      , InfixL (EBinOp OpSub <$ symbol "-")
      ]
    , [ InfixR (EBinOp OpConcat <$ symbol "++")
      , InfixR (EBinOp OpCons <$ symbol ":")
      ]
    , [ InfixN (EBinOp OpEq <$ symbol "==")
      , InfixN (EBinOp OpNeq <$ symbol "/=")
      , InfixN (EBinOp OpLe <$ symbol "<=")
      , InfixN (EBinOp OpGe <$ symbol ">=")
      , InfixN (EBinOp OpLt <$ try (symbol "<" <* notFollowedBy (char '=')))
      , InfixN (EBinOp OpGt <$ try (symbol ">" <* notFollowedBy (char '=')))
      ]
    , [ InfixR (EBinOp OpAnd <$ symbol "&&") ]
    , [ InfixR (EBinOp OpOr <$ symbol "||") ]
    ]

pAppExpr :: Parser Expr
pAppExpr = foldl1 EApp <$> some pAtomExpr

pAtomExpr :: Parser Expr
pAtomExpr = choice
    [ ELit <$> pLit
    , pTupleExpr
    , pListExpr
    , EVar <$> pId
    ]

pTupleExpr :: Parser Expr
pTupleExpr = parens (pExpr >>= \first -> option first (mkTuple first))
  where
    mkTuple first = ETuple . (first :) <$> some (symbol "," *> pExpr)

pListExpr :: Parser Expr
pListExpr = EList <$> brackets (pExpr `sepBy` symbol ",")

-- ============ Id and Literal Parser ============
pId :: Parser Id
pId = Id <$> lexeme ((:) <$> lowerChar <*> many idChar)
  where
    idChar = alphaNumChar <|> char '_' <|> char '\''

pLit :: Parser Lit
pLit = choice
    [ LBool True <$ symbol "true"
    , LBool False <$ symbol "false"
    , LChar <$> pCharLit
    , LStr <$> pStrLit
    , LInt <$> pIntLit
    ]

pIntLit :: Parser Integer
pIntLit = lexeme L.decimal

pCharLit :: Parser Char
pCharLit = lexeme (char '\'' *> pStrChar <* char '\'')

pStrLit :: Parser String
pStrLit = lexeme (char '"' *> many pStrChar <* char '"')

pStrChar :: Parser Char
pStrChar = pEscapeChar <|> noneOf ['\"', '\\']
  where
    pEscapeChar = char '\\' *> choice
        [ '\n' <$ char 'n'
        , '\t' <$ char 't'
        , '\r' <$ char 'r'
        , '\\' <$ char '\\'
        , '\"' <$ char '"'
        , '\'' <$ char '\''
        ]

-- ============ Public Interface ============

parseProgram :: Text -> Either String Program
parseProgram input =
    case parse pProgram "<input>" input of
        Left err -> Left (errorBundlePretty err)
        Right prog -> Right prog