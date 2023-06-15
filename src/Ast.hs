module Ast where

import Data.List (intercalate)
import Text.Printf (printf)

data Type
  = TypeFunc [Type] Type
  | TypeSymbol String
  | TypeObj [(String, Type)]
  | TypeVar String Int
  deriving (Eq, Ord)

data Expr
  = ExprCall Expr [Expr]
  | ExprFunc [(String, Type)] Type Expr
  | ExprLabel String
  | ExprObj [(String, Expr)]
  | ExprSymbol String

data Stmt
  = StmtBinding String Expr
  | StmtVoid Expr

instance Show Type where
  show (TypeFunc args returnType) =
    printf "(%s) -> %s" (commaDelim $ map show args) (show returnType)
  show (TypeSymbol symbol) = symbol
  show (TypeObj []) = printf "{}"
  show (TypeObj pairs) =
    printf "{ %s }" $ commaDelim $ map (uncurry showKeyValue) pairs
  show (TypeVar var k) = printf "'%s%d" var k

instance Show Expr where
  show (ExprCall func []) =
    printf "(%s)" $ show func
  show (ExprCall func args) =
    printf "(%s %s)" (show func) (unwords $ map show args)
  show (ExprFunc args returnType returnExpr) =
    printf
      "\\(%s) -> %s = %s"
      (commaDelim $ map (uncurry showKeyValue) args)
      (show returnType)
      (show returnExpr)
  show (ExprLabel label) = label
  show (ExprObj pairs) =
    printf "{ %s }" $ commaDelim $ map (uncurry showKeyValue) pairs
  show (ExprSymbol symbol) = symbol

instance Show Stmt where
  show (StmtBinding label expr) = printf "%s = %s" label (show expr)
  show (StmtVoid expr) = show expr

commaDelim :: [String] -> String
commaDelim = intercalate ", "

showKeyValue :: Show a => String -> a -> String
showKeyValue key = printf "%s: %s" key . show
