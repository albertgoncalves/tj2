module Ast where

import Data.Bifunctor (bimap)
import Data.List (intercalate)
import Text.Printf (printf)

data Type
  = TypeFunc Int [TypeOffset] TypeOffset
  | TypeSymbol String
  | TypeObj [(StringOffset, TypeOffset)]
  | TypeVar String Int

data Expr
  = ExprCall ExprOffset [ExprOffset]
  | ExprFunc Int [(StringOffset, TypeOffset)] TypeOffset ExprOffset
  | ExprLabel String
  | ExprObj [(StringOffset, ExprOffset)]
  | ExprSymbol String

data Stmt
  = StmtBinding String ExprOffset
  | StmtVoid ExprOffset

type StringOffset = (Int, String)

type TypeOffset = (Int, Type)

type ExprOffset = (Int, Expr)

type StmtOffset = (Int, Stmt)

instance Show Type where
  show (TypeFunc _ args (_, returnType)) =
    printf "(%s) -> %s" (commaDelim $ map (show . snd) args) (show returnType)
  show (TypeSymbol symbol) = symbol
  show (TypeObj []) = printf "{}"
  show (TypeObj pairs) =
    printf "{ %s }" $ commaDelim $ map (uncurry showKeyValue) pairs
  show (TypeVar var _) = printf "'%s" var

instance Eq Type where
  (TypeFunc _ leftArgs (_, leftReturn))
    == (TypeFunc _ rightArgs (_, rightReturn)) =
      (map snd leftArgs == map snd rightArgs) && (leftReturn == rightReturn)
  (TypeSymbol leftSymbol) == (TypeSymbol rightSymbol) =
    leftSymbol == rightSymbol
  (TypeObj leftPairs) == (TypeObj rightPairs) =
    map (bimap snd snd) leftPairs == map (bimap snd snd) rightPairs
  (TypeVar leftVar leftK) == (TypeVar rightVar rightK) =
    (leftVar == rightVar) && (leftK == rightK)
  _ == _ = False

instance Show Expr where
  show (ExprCall (_, func) []) =
    printf "(%s)" $ show func
  show (ExprCall (_, func) args) =
    printf "(%s %s)" (show func) (unwords $ map (show . snd) args)
  show (ExprFunc _ args (_, returnType) (_, returnExpr)) =
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
  show (StmtBinding label (_, expr)) = printf "%s = %s" label (show expr)
  show (StmtVoid (_, expr)) = show expr

commaDelim :: [String] -> String
commaDelim = intercalate ", "

showKeyValue :: Show b => StringOffset -> (a, b) -> String
showKeyValue (_, key) = printf "%s: %s" key . show . snd
