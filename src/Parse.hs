module Parse where

import Ast (Expr (..), Stmt (..), Type (..))
import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Text.ParserCombinators.ReadP
  ( ReadP,
    char,
    eof,
    get,
    look,
    munch,
    munch1,
    pfail,
    readP_to_S,
    satisfy,
    string,
    (<++),
  )

many :: ReadP a -> ReadP [a]
many p = many1 p <++ return []

many1 :: ReadP a -> ReadP [a]
many1 p = (:) <$> p <*> many p

sepBy :: ReadP a -> ReadP sep -> ReadP [a]
sepBy p sep = sepBy1 p sep <++ return []

sepBy1 :: ReadP a -> ReadP sep -> ReadP [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

choice :: [ReadP a] -> ReadP a
choice [] = pfail
choice [p] = p
choice (p : ps) = p <++ choice ps

space :: ReadP ()
space = do
  rest <- look
  case rest of
    ('#' : _) -> do
      _ <- get
      munch (/= '\n') *> ((char '\n' *> space) <++ eof)
    (c : _) | isSpace c -> do
      _ <- get
      space
    _ -> return ()

token :: ReadP a -> ReadP a
token = (space *>)

lowerIdent :: ReadP String
lowerIdent = token $ (:) <$> satisfy isLower <*> munch isAlphaNum

upperIdent :: ReadP String
upperIdent = token $ (:) <$> satisfy isUpper <*> munch isAlphaNum

comma :: ReadP Char
comma = token $ char ','

keyValues :: ReadP a -> ReadP [(String, a)]
keyValues p = sepBy ((,) <$> (lowerIdent <* token (char ':')) <*> p) comma

leftParen :: ReadP Char
leftParen = token $ char '('

rightParen :: ReadP Char
rightParen = token $ char ')'

leftBrace :: ReadP Char
leftBrace = token $ char '{'

rightBrace :: ReadP Char
rightBrace = token $ char '}'

arrow :: ReadP String
arrow = token $ string "->"

typeFunc :: ReadP Type
typeFunc = do
  _ <- leftParen
  args <- sepBy type' comma
  _ <- rightParen
  _ <- arrow
  TypeFunc args <$> type'

typeSymbol :: ReadP Type
typeSymbol = TypeSymbol <$> upperIdent

typeObj :: ReadP Type
typeObj = do
  _ <- leftBrace
  pairs <- sepBy1 ((,) <$> (lowerIdent <* token (char ':')) <*> type') comma
  _ <- rightBrace
  return $ TypeObj pairs

typeVar :: ReadP Type
typeVar =
  TypeVar
    <$> (token (char '\'') *> munch1 isLower)
    <*> (read <$> munch1 isDigit)

type' :: ReadP Type
type' = choice [typeFunc, typeSymbol, typeObj, typeVar]

exprCall :: ReadP Expr
exprCall = do
  _ <- leftParen
  func <- expr
  args <- many expr
  _ <- rightParen
  return $ ExprCall func args

exprFunc :: ReadP Expr
exprFunc = do
  _ <- token $ char '\\'
  _ <- leftParen
  args <- keyValues type'
  _ <- rightParen
  _ <- arrow
  returnType <- type'
  _ <- token $ char '='
  ExprFunc args returnType <$> expr

exprLabel :: ReadP Expr
exprLabel = ExprLabel <$> lowerIdent

exprObj :: ReadP Expr
exprObj = do
  _ <- leftBrace
  pairs <- keyValues expr
  _ <- rightBrace
  return $ ExprObj pairs

exprSymbol :: ReadP Expr
exprSymbol = ExprSymbol <$> upperIdent

expr :: ReadP Expr
expr = choice [exprCall, exprFunc, exprLabel, exprObj, exprSymbol]

stmtBinding :: ReadP Stmt
stmtBinding = do
  label <- lowerIdent
  _ <- token $ char '='
  StmtBinding label <$> expr

stmtVoid :: ReadP Stmt
stmtVoid = StmtVoid <$> expr

stmt :: ReadP Stmt
stmt = choice [stmtBinding, stmtVoid]

parse :: String -> [Stmt]
parse = fst . head . readP_to_S (many1 stmt <* token eof)
